(* The MIT License (MIT)

   Copyright (c) 2014-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

let src = Logs.Src.create "maildir" ~doc:"logs maildir's event"
module Log = (val Logs.src_log src : Logs.LOG)

type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

let compare_flag a b = match a, b with
  | NEW, NEW
  | SEEN, SEEN
  | REPLIED, REPLIED
  | FLAGGED, FLAGGED
  | TRASHED, TRASHED
  | PASSED, PASSED
  | DRAFT, DRAFT -> 0
  | NEW, _ -> 1
  | SEEN, _ -> 1
  | REPLIED, _ -> 1
  | FLAGGED, _ -> 1
  | TRASHED, _ -> 1
  | PASSED, _ -> 1
  | DRAFT, _ -> 1

let char_of_flag = function
  | SEEN -> Some 'S'
  | REPLIED -> Some 'R'
  | FLAGGED -> Some 'F'
  | TRASHED -> Some 'T'
  | PASSED -> Some 'P'
  | DRAFT -> Some 'D'
  | NEW -> None

let string_of_flags flags =
  let l =
    List.fold_right (fun f l ->
      match char_of_flag f with Some c -> c :: l | None -> l
    ) flags []
  in
  let l = List.sort_uniq compare l in
  let b = Bytes.create (List.length l) in
  List.iteri (fun i c -> Bytes.set b i c) l;
  Bytes.unsafe_to_string b

let canonicalize_flags flags =
  let module Set = Set.Make(struct type t = flag let compare = compare_flag end) in
  let set = List.fold_right Set.add flags Set.empty in
  Set.elements set

let pp_flags = Fmt.using string_of_flags Fmt.string

type 'a uniq_flag =
  | Seq : int64 uniq_flag
  | X : int64 uniq_flag
  | R : int64 uniq_flag
  | I : int64 uniq_flag
  | V : int64 uniq_flag
  | M : int64 uniq_flag
  | P : int uniq_flag
  | Q : int uniq_flag

type v_uniq_flag = V : 'a uniq_flag -> v_uniq_flag

type uniq =
  { sequence : int64 option
  ; boot : int64 option
  ; crypto_random : int64 option
  ; inode : int64 option
  ; device : int64 option
  ; microsecond : int64 option
  ; pid : int option
  ; deliveries : int option
  ; order : v_uniq_flag list }

let value_of_uniq_flag : type a. a uniq_flag -> uniq -> a option = fun uniq_flag t -> match uniq_flag with
  | Seq -> t.sequence
  | X -> t.boot
  | R -> t.crypto_random
  | I -> t.inode
  | V -> t.device
  | M -> t.microsecond
  | P -> t.pid
  | Q -> t.deliveries

let pp_of_uniq_flag : type a. a uniq_flag -> a Fmt.t = fun uniq_flag ppf v -> match uniq_flag with
  | Seq -> Fmt.pf ppf "#%Lx" v
  | X -> Fmt.pf ppf "X%Lx" v
  | R -> Fmt.pf ppf "R%Lx" v
  | I -> Fmt.pf ppf "I%Lx" v
  | V -> Fmt.pf ppf "V%Lx" v
  | M -> Fmt.pf ppf "M%Ld" v
  | P -> Fmt.pf ppf "P%d" v
  | Q -> Fmt.pf ppf "Q%d" v

let pp_uniq ppf t =
  List.iter (fun (V uniq_flag) -> (Fmt.option (pp_of_uniq_flag uniq_flag)) ppf (value_of_uniq_flag uniq_flag t)) t.order

let default_uniq =
  { sequence = None
  ; boot = None
  ; crypto_random = None
  ; inode = None
  ; device = None
  ; microsecond = None
  ; pid = None
  ; deliveries = None
  ; order = [] }

type uid =
  | Modern of uniq
  | Old0 of int
  | Old1 of int * int

let pp_uid ppf = function
  | Modern uniq -> Fmt.pf ppf "%a" pp_uniq uniq
  | Old0 n -> Fmt.int ppf n
  | Old1 (n, m) -> Fmt.pf ppf "%d_%d" n m

type info =
  | Info of flag list

let pp_info ppf = function
  | Info [] | Info [ NEW ] -> Fmt.nop ppf ()
  | Info flags -> pp_flags ppf flags

type message =
  { time : int64
  ; uid : uid
  ; info : info
  ; host : string
  ; parameters : (string * string) list }

let is_new { info = Info flags; _ } = List.exists ((=) NEW) flags

let sanitize_host host =
  let len = String.length host in
  let res = Buffer.create len in
  for i = 0 to len - 1 do
    match host.[i] with
    | '/' -> Buffer.add_string res "\\057"
    | ':' -> Buffer.add_string res "\\072"
    | c -> Buffer.add_char res c
  done;
  Buffer.contents res

let pp_host = Fmt.using sanitize_host Fmt.string

let pp_parameter ppf (k, v) =
  Fmt.pf ppf "%s=%s" k v

let pp_message ppf t =
  if is_new t
  then
    Fmt.pf ppf "%Ld.%a.%a%a"
      t.time pp_uid t.uid pp_host t.host
      Fmt.(iter List.iter (prefix (const char ',') pp_parameter)) t.parameters
  else
    Fmt.pf ppf "%Ld.%a.%a%a:2,%a"
      t.time pp_uid t.uid pp_host t.host
      Fmt.(iter List.iter (prefix (const char ',') pp_parameter)) t.parameters
      pp_info t.info

type filename = string

let to_filename = Fmt.to_to_string pp_message

let with_new message =
  let Info flags = message.info in
  if List.exists (function NEW -> true | _ -> false) flags
  then message
  else { message with info = Info (NEW :: flags) }

let without_new ?(flags = []) message =
  let Info flags' = message.info in
  let flags = List.merge compare_flag flags flags' in
  let flags = List.filter (function NEW -> false | _ -> true) flags in
  { message with info = Info flags }

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_hexadigit = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
  let number = take_while1 is_digit >>| Int64.of_string
  let hexanumber = take_while1 is_hexadigit >>| fun str -> Int64.of_string ("0x" ^ str)

  let sequence = char '#' *> hexanumber
  let boot = char 'X' *> hexanumber
  let crypto_random = char 'R' *> hexanumber
  let inode = char 'I' *> hexanumber
  let device = char 'V' *> hexanumber
  let microsecond = char 'M' *> number
  let pid = char 'P' *> number
  let deliveries = char 'Q' *> number

  let modern =
    choice
      [ (sequence >>| fun x -> `Sequence x)
      ; (boot >>| fun x -> `Boot x)
      ; (crypto_random >>| fun x -> `Crypto_random x)
      ; (inode >>| fun x -> `Inode x)
      ; (device >>| fun x -> `Device x)
      ; (microsecond >>| fun x -> `Microsecond x)
      ; (pid >>| fun x -> `Pid x)
      ; (deliveries >>| fun x -> `Deliveries x) ]

  let modern =
    many1 modern >>| fun lst -> List.fold_left
    (fun t -> function
      | `Sequence x -> { t with sequence = Some x
                              ; order = V Seq :: t.order }
      | `Boot x -> { t with boot = Some x
                          ; order = V X :: t.order }
      | `Crypto_random x -> { t with crypto_random = Some x
                                   ; order = V R :: t.order }
      | `Inode x -> { t with inode = Some x
                           ; order = V I :: t.order }
      | `Device x -> { t with device = Some x
                            ; order = V V :: t.order }
      | `Microsecond x -> { t with microsecond = Some x
                                 ; order = V M :: t.order }
      | `Pid x -> { t with pid = Some (Int64.to_int x)
                         ; order = V P :: t.order }
      | `Deliveries x -> { t with deliveries = Some (Int64.to_int x)
                                ; order = V Q :: t.order })
    default_uniq lst |> fun uniq -> { uniq with order = List.rev uniq.order }

  let old1 = (number >>| Int64.to_int) <* char '_' >>= fun n -> (number >>| Int64.to_int) >>= fun m -> return (n, m)
  let old0 = (number >>| Int64.to_int)

  let uid =
    choice
      [ (modern >>| fun x -> Modern x)
      ; (old1 >>| fun (n, m) -> Old1 (n, m))
      ; (old0 >>| fun x -> Old0 x) ]

  let host =
    take_while1 (function ':' -> false | _ -> true)
    >>| fun str ->
    let len = String.length str in
    let res = Buffer.create len in
    let idx = ref 0 in
    let has_backslash = ref false in

    while !idx < len
    do
      if !has_backslash && (!idx - len) >= 3
      then match String.sub str !idx 3 with
        | "057" -> Buffer.add_char res '/' ; has_backslash := false ; idx := !idx + 3
        | "072" -> Buffer.add_char res ':' ; has_backslash := false ; idx := !idx + 3
        | _ -> Buffer.add_char res str.[!idx] ; has_backslash := str.[!idx] = '\\' ; incr idx
      else ( Buffer.add_char res str.[!idx] ;
             has_backslash := str.[!idx] = '\\' ;
             incr idx )
    done ;

    Buffer.contents res

  let parameter =
    take_while1 (function '=' | ':' | ',' -> false | _ -> true) <* char '=' >>= fun key ->
    take_while1 (function '=' | ':' | ',' -> false | _ -> true) >>= fun value -> return (key, value)

  let flag =
    choice
      [ (char 'P' *> return PASSED)
      ; (char 'R' *> return REPLIED)
      ; (char 'S' *> return SEEN)
      ; (char 'T' *> return TRASHED)
      ; (char 'D' *> return DRAFT)
      ; (char 'F' *> return FLAGGED) ]

  let failf fmt = Fmt.kstrf fail fmt

  let filename =
    take_while1 is_digit >>| Int64.of_string >>= fun time ->
    char '.' *> uid >>= fun uid ->
    char '.' *> host >>= fun host ->
    many (char ',' *> parameter) >>= fun parameters ->
    peek_char >>= function
    | None ->
        return { time; uid; host; parameters; info = Info [ NEW ] }
    | Some ':' ->
        (char ':' *> peek_char >>= function
          (* TODO: | Some '1' -> _ *)
          | Some '2' ->
              char '2' *> char ',' *> many flag >>| fun flags -> { time; uid; host; parameters; info = Info flags }
          | None -> failf "Expect more input"
          | Some chr -> failf "Invalid character: %02x" (Char.code chr))
    | Some chr -> failf "Invalid character: %02x" (Char.code chr)

  let of_filename input =
    match parse_string filename input with
    | Ok v -> Ok v
    | Error err -> Rresult.R.error_msgf "Invalid filename: %s (%s)" input err
end

let of_filename = Parser.of_filename

type t =
  { pid : int
  ; host : string
  ; path : Fpath.t
  ; random : unit -> int64
  ; mutable mtime_new : int64
  ; mutable mtime_cur : int64
  ; mutable delivered : int }

let to_fpath t message =
  let result = to_filename message in
  if is_new message
  then Fpath.(t.path / "new" / result)
  else Fpath.(t.path / "cur" / result)

let create ~pid ~host ~random path =
  { pid
  ; host= sanitize_host host
  ; random
  ; path
  ; mtime_new = -1L
  ; mtime_cur = -1L
  ; delivered = 0 }

let new_message ~time t =
  let message =
    { time
    ; uid= Modern { default_uniq with crypto_random= Some (t.random ())
                                    ; pid= Some t.pid
                                    ; deliveries= Some t.delivered
                                    ; order = [ V R; V P; V Q ] }
    ; host= t.host
    ; info= Info [ NEW ]
    ; parameters= [] } in
  t.delivered <- t.delivered + 1 ;
  message

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end

module type FS = sig
  type key
  type t

  type +'a io

  val mtime : t -> key -> int64 io
  val fold : t -> key -> (key -> 'a -> 'a io) -> 'a -> 'a io
  val rename : t -> key -> key -> unit io
  val remove : t -> key -> unit io
  val exists : t -> key -> bool io
end

module Make
    (IO : IO)
    (FS : FS with type +'a io = 'a IO.t and type key = Fpath.t) = struct
  open IO

  type ('a, 'b) transmit = FS.t -> ('a, 'b) result IO.t

  let verify fs t =
    FS.exists fs Fpath.(t.path / "cur" / "") >>= fun cur_ ->
    FS.exists fs Fpath.(t.path / "tmp" / "") >>= fun tmp_ ->
    FS.exists fs Fpath.(t.path / "new" / "") >>= fun new_ ->
    return (cur_ && tmp_ && new_)

  let add fs t ~time transmit =
    let uid = new_message ~time t in
    let message = to_filename uid in
    let transmit = transmit Fpath.(t.path / "tmp" / message) in
    transmit fs >>= function
    | Ok v ->
        let o = Fpath.(t.path / "tmp" / message) in
        let n = Fpath.(t.path / "new" / message) in
        FS.rename fs o n >>= fun () ->
        return (Ok v)
    | Error _ as err -> return err

  let scan_only_new computation acc fs t =
    FS.mtime fs Fpath.(t.path / "new" / "")
    >>= fun mtime_new ->
    (if mtime_new > t.mtime_new
     then ( t.mtime_new <- mtime_new
          ; return true )
     else return false)
    >>= fun new_changed ->
    FS.mtime fs Fpath.(t.path / "cur" / "")
    >>= fun mtime_cur ->
    (if mtime_cur > t.mtime_cur
     then ( t.mtime_cur <- mtime_cur
          ; return true )
     else return false)
    >>= fun cur_changed ->
    if new_changed || cur_changed
    then
      let computation_new path acc =
        let filename = Fpath.basename path in
        match Parser.of_filename filename with
        | Ok v -> computation acc (with_new v)
        | Error (`Msg err) ->
            Log.warn (fun m -> m "%s" err) ;
            return acc in
      let computation_cur path acc =
        let filename = Fpath.basename path in
        match Parser.of_filename filename with
        | Ok v -> computation acc v
        | Error (`Msg err) ->
            Log.warn (fun m -> m "%s" err) ;
            return acc in
      FS.fold fs Fpath.(t.path / "new" / "") computation_new acc >>= fun acc ->
      FS.fold fs Fpath.(t.path / "cur" / "") computation_cur acc >>= fun acc ->
      return acc
    else return acc

  let commit fs t ?(flags = []) message =
    if List.exists (function NEW -> true | _ -> false) flags
    then Fmt.invalid_arg "Impossible to apply NEW flag on a committed message." ;

    if is_new message
    then
      let message' = without_new ~flags message in
      let m = to_filename message in
      let m' = to_filename message' in
      let a = Fpath.(t.path / "new" / m) in
      let b = Fpath.(t.path / "cur" / m') in
      FS.rename fs a b
    else return ()

  let fold computation acc fs t =
    let computation_new path acc =
      let filename = Fpath.basename path in
      match Parser.of_filename filename with
      | Ok v -> computation acc (with_new v)
      | Error (`Msg err) ->
          Log.warn (fun m -> m "%s" err) ;
          return acc in
    let computation_cur path acc =
      let filename = Fpath.basename path in
      match Parser.of_filename filename with
      | Ok v -> computation acc v
      | Error (`Msg err) ->
          Log.warn (fun m -> m "%s" err) ;
          return acc in
    FS.fold fs Fpath.(t.path / "new" / "") computation_new acc >>= fun acc ->
    FS.fold fs Fpath.(t.path / "cur" / "") computation_cur acc >>= fun acc ->
    return acc

  let get t uid =
    let message = to_filename uid in
    if is_new uid
    then Fpath.(t.path / "new" / message)
    else Fpath.(t.path / "cur" / message)

  let remove fs t uid =
    let message = to_filename uid in
    if is_new uid
    then FS.remove fs Fpath.(t.path / "new" / message)
    else FS.remove fs Fpath.(t.path / "cur" / message)

  let get_flags fs t uid =
    let message = to_filename uid in
    let directory = if is_new uid then "new" else "cur" in
    FS.exists fs Fpath.(t.path / directory / message) >>= function
    | true ->
        let Info flags = uid.info in
        return flags
    | false -> return []

  let set_flags fs t uid flags' =
    let message = to_filename uid in
    let directory = if is_new uid then "new" else "cur" in
    FS.exists fs Fpath.(t.path / directory / message) >>= function
    | false -> return ()
    | true ->
        let Info flags = uid.info in
        let flags = canonicalize_flags flags in
        let flags' = canonicalize_flags flags' in
        if flags <> flags'
        then
          let message' = to_filename { uid with info = Info flags' } in
          FS.rename fs Fpath.(t.path / directory / message) Fpath.(t.path / directory / message')
        else return ()
end
