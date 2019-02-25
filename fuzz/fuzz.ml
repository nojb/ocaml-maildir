open Crowbar

module Option = struct
  type 'a t = 'a option

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let is_none = function
    | Some _ -> false
    | None -> true
end

let with_hex_int64_raw x = (x, Fmt.strf "%016Lx" x)
let with_int64_raw x = (x, Fmt.strf "%Ld" x)
let with_int_raw x = (x, Fmt.strf "%d" x)

let ( >>= ) = dynamic_bind

let equal_flag (Maildir.V a) (Maildir.V b) = match a, b with
  | Maildir.Seq, Maildir.Seq -> true
  | Maildir.X, Maildir.X -> true
  | Maildir.R, Maildir.R -> true
  | Maildir.I, Maildir.I -> true
  | Maildir.V, Maildir.V -> true
  | Maildir.M, Maildir.M -> true
  | Maildir.P, Maildir.P -> true
  | Maildir.Q, Maildir.Q -> true
  | _, _ -> false

let flag_to_int (Maildir.V x) = match x with
  | Maildir.Seq -> 0b1
  | Maildir.X -> 0b10
  | Maildir.R -> 0b100
  | Maildir.I -> 0b1000
  | Maildir.V -> 0b10000
  | Maildir.M -> 0b100000
  | Maildir.P -> 0b1000000
  | Maildir.Q -> 0b10000000

let order = fun (seq, x, r, i, v, m, p, q) ->
  let flags =
    (Option.map (fun _ -> Maildir.(V Seq)) seq)
    :: (Option.map (fun _ -> Maildir.(V X)) x)
    :: (Option.map (fun _ -> Maildir.(V R)) r)
    :: (Option.map (fun _ -> Maildir.(V I)) i)
    :: (Option.map (fun _ -> Maildir.(V V)) v)
    :: (Option.map (fun _ -> Maildir.(V M)) m)
    :: (Option.map (fun _ -> Maildir.(V P)) p)
    :: (Option.map (fun _ -> Maildir.(V Q)) q)
    :: [] in
  let flags = List.fold_left (fun a -> function Some x -> x :: a | None -> a) [] flags in

  if List.length flags = 0 then bad_test () ;

  let choose = Crowbar.choose (List.map (fun flag -> Crowbar.const flag) flags) in
  Crowbar.map [ list1 choose ] @@ fun order ->
  let order, _ =
    List.fold_left
      (fun (a, s) x -> if s land flag_to_int x <> 0 then (a, s) else (x :: a, s lor flag_to_int x))
      ([], 0) order in
  let order =
    List.fold_left
      (fun a x -> if List.exists (equal_flag x) a then a else x :: a)
      order flags in
  (seq, x, r, i, v, m, p, q, order)

let gen_uniq = map
    [ option int64
    ; option int64
    ; option int64
    ; option int64
    ; option int64
    ; option int64
    ; option int
    ; option int ] (fun seq x r i v m p q -> (Option.map Int64.abs seq,
                                              Option.map Int64.abs x,
                                              Option.map Int64.abs r,
                                              Option.map Int64.abs i,
                                              Option.map Int64.abs v,
                                              Option.map Int64.abs m,
                                              Option.map abs p,
                                              Option.map abs q))

let gen_uniq = gen_uniq >>= order

let gen_uniq =
  map [ gen_uniq ]
  @@ fun (seq, x, r, i, v, m, p, q, order) ->
  if Option.is_none seq
  && Option.is_none x
  && Option.is_none r
  && Option.is_none i
  && Option.is_none v
  && Option.is_none m
  && Option.is_none p
  && Option.is_none q
  then bad_test ()
  else { Maildir.sequence= Option.map with_hex_int64_raw seq
       ; boot= Option.map with_hex_int64_raw x
       ; crypto_random= Option.map with_hex_int64_raw r
       ; inode= Option.map with_hex_int64_raw i
       ; device= Option.map with_hex_int64_raw v
       ; microsecond= Option.map with_int64_raw m
       ; pid= Option.map with_int_raw p
       ; deliveries= Option.map with_int_raw q
       ; order }

let gen_uid = choose [ map [ gen_uniq] (fun uniq -> Maildir.Modern uniq)
                     ; map [ int ] (fun n -> Maildir.Old0 (abs n))
                     ; map [ int; int ] (fun n m -> Maildir.Old1 (abs n, abs m)) ]

let gen_flag =
  choose
    [ const Maildir.SEEN
    ; const Maildir.REPLIED
    ; const Maildir.FLAGGED
    ; const Maildir.TRASHED
    ; const Maildir.PASSED
    ; const Maildir.DRAFT
    ; const Maildir.NEW ]

let int_of_flag = function
  | Maildir.SEEN -> 0b1
  | Maildir.REPLIED -> 0b10
  | Maildir.FLAGGED -> 0b100
  | Maildir.TRASHED -> 0b1000
  | Maildir.PASSED -> 0b10000
  | Maildir.DRAFT -> 0b100000
  | Maildir.NEW -> 0b1000000

let gen_flags = map [ list gen_flag ] @@ fun flags ->
  let flags, _ =
    List.fold_left
      (fun (a, s) x -> if s land int_of_flag x <> 0 then (a, s) else (x :: a, s lor int_of_flag x))
      ([], 0) flags in
  flags

let gen_info = map [ gen_flags ] @@ fun flags -> Maildir.Info flags

let ( <.> ) f g = fun x -> f (g x)

let char_from_alphabet alphabet =
  map [ range (String.length alphabet) ] (String.make 1 <.> String.get alphabet)

let string_from_alphabet alphabet len =
  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (char_from_alphabet alphabet :: acc) (pred n) in
  go [] len

let alphabet_from_predicate predicate =
  let len =
    let rec go acc = function
      | 0 -> if predicate (Char.unsafe_chr 0) then acc + 1 else acc
      | n ->
        let acc = if predicate (Char.unsafe_chr n) then acc + 1 else acc in
        go acc (n - 1) in
    go 0 255 in
  let res = Bytes.create len in
  let rec go idx = function
    | 0 ->
      if predicate (Char.unsafe_chr 0) then Bytes.unsafe_set res idx (Char.unsafe_chr 0)
    | n ->
      if predicate (Char.unsafe_chr n) then Bytes.unsafe_set res idx (Char.unsafe_chr n) ;
      let idx = if predicate (Char.unsafe_chr n) then idx + 1 else idx in
      go idx (n - 1) in
  go 0 255 ; Bytes.unsafe_to_string res

let is_obs_no_ws_ctl = function
  | '\032' .. '\126' -> true
  | _ -> false
let is_dtext = function
  | '\044' (* , *) -> false
  | '\033' .. '\090' | '\094' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c
let is_key = function
  | '=' | ':' | ',' | '\000' .. '\031' | '\127' -> false
  | chr -> Char.code chr < 128
let is_value = is_key

let dtext = alphabet_from_predicate is_dtext
let key = alphabet_from_predicate is_key
let value = alphabet_from_predicate is_value

let gen_host = (range ~min:1 78) >>= (string_from_alphabet dtext)
let gen_key = (range ~min:1 78) >>= (string_from_alphabet key)
let gen_value = (range ~min:1 78) >>= (string_from_alphabet value)
let gen_parameter = map [ gen_key; gen_value ] @@ fun key value -> (key, value)

let gen_message =
  map [ int64; gen_uid; gen_info; gen_host; list gen_parameter ]
    (fun time uid info host parameters -> { Maildir.time= Int64.abs time
                                          ; uid
                                          ; info
                                          ; host
                                          ; parameters })

let pp_flag ppf = function
  | Maildir.SEEN -> Fmt.string ppf "Seen"
  | Maildir.NEW -> Fmt.string ppf "New"
  | Maildir.REPLIED -> Fmt.string ppf "Replied"
  | Maildir.FLAGGED -> Fmt.string ppf "Replied"
  | Maildir.TRASHED -> Fmt.string ppf "Trashed"
  | Maildir.PASSED -> Fmt.string ppf "Passed"
  | Maildir.DRAFT -> Fmt.string ppf "Draft"

let pp_info ppf (Maildir.Info flags) = Fmt.(Dump.list pp_flag) ppf flags

let pp_uniq_flag ppf (Maildir.V flag) = match flag with
  | Maildir.Seq -> Fmt.string ppf "Seq"
  | Maildir.X -> Fmt.string ppf "X"
  | Maildir.R -> Fmt.string ppf "R"
  | Maildir.I -> Fmt.string ppf "I"
  | Maildir.V -> Fmt.string ppf "V"
  | Maildir.M -> Fmt.string ppf "M"
  | Maildir.P -> Fmt.string ppf "P"
  | Maildir.Q -> Fmt.string ppf "Q"

let pp_uniq ppf uniq =
  let open Maildir in
  Fmt.pf ppf "{ @[<hov>sequence = %a;@ \
                       boot = %a;@ \
                       crypto_random = %a;@ \
                       inode = %a;@ \
                       device = %a;@ \
                       microsecond = %a;@ \
                       pid = %a;@ \
                       deliveries = %a;@ \
                       order = %a;@] }"
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.sequence
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.boot
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.crypto_random
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.inode
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.device
    Fmt.(Dump.option (Dump.pair int64 string)) uniq.microsecond
    Fmt.(Dump.option (Dump.pair int string)) uniq.pid
    Fmt.(Dump.option (Dump.pair int string)) uniq.deliveries
    Fmt.(Dump.list pp_uniq_flag) uniq.order

let pp_uid ppf = function
  | Maildir.Modern uniq -> Fmt.pf ppf "(Modern %a)" pp_uniq uniq
  | Maildir.Old0 n -> Fmt.pf ppf "(Old0 %d)" n
  | Maildir.Old1 (n, m) -> Fmt.pf ppf "(Old1 %a)" Fmt.(Dump.pair int int) (n, m)

let pp_message ppf message =
  Fmt.pf ppf "{ @[<hov>time = %Ld;@ \
                       uid = %a;@ \
                       info = %a;@ \
                       host = %s;@ \
                       parameters = %a@] }"
    message.Maildir.time
    (Fmt.hvbox pp_uid) message.Maildir.uid
    (Fmt.hvbox pp_info) message.Maildir.info
    message.Maildir.host
    Fmt.(Dump.list (Dump.pair string string)) message.Maildir.parameters

let pp_chr =
  let escaped = function ' ' .. '~' as c -> String.make 1 c | _ -> "." in
  Fmt.using escaped Fmt.string

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp = pp_scalar ~get:String.get ~length:String.length

let () =
  add_test ~name:"maildir" [ gen_message ]
  @@ fun message ->
  let str = Maildir.to_filename message in

  Fmt.pr "> result: @[<hov>%a@].\n%!" pp str ;
  Fmt.pr "> for: @[<hov>%a@].\n%!" pp_message message ;

  match Maildir.of_filename str with
  | Ok res ->
      check_eq ~pp:Fmt.(Dump.list (pair string string)) ~eq:Maildir.equal_parameters message.Maildir.parameters res.Maildir.parameters ;
      check_eq ~pp:Maildir.pp_info ~eq:Maildir.equal_info message.Maildir.info res.Maildir.info ;
      check_eq ~pp:Fmt.string ~eq:String.equal message.Maildir.host res.Maildir.host ;
      check_eq ~pp:Maildir.pp_uid ~eq:Maildir.equal_uid message.Maildir.uid res.Maildir.uid ;
      check_eq ~pp:pp_message ~eq:Maildir.equal_message message res
  | Error (`Msg err) -> fail err
