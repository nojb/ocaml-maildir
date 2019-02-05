let () = Printexc.record_backtrace true

module Store = Maildir_mem

(* XXX(dinosaure): it's bad but I know that `Maildir_mem.fs` is an `Hashtbl.t`. *)
type elt = { mutable mtime : int64; contents : [ `Directory | `Contents of string ]; }

let pp_elt ppf t =
  let pp_contents ppf = function
    | `Directory -> Fmt.string ppf "#directory"
    | `Contents _ -> Fmt.string ppf "#message" in
  Fmt.pf ppf "{ @[<hov>mtime = %Ld;@ contents = %a;@] }"
    t.mtime pp_contents t.contents

external to_fs : (Store.FS.key, elt) Hashtbl.t -> Store.fs = "%identity"

let store : (Store.FS.key, elt) Hashtbl.t = Hashtbl.create 128

let ( <.> ) f g = fun x -> f (g x)

let gettimeofday = Store.gettime

let ex_nihilo = Fpath.v "ex_nihilo"

let fs = to_fs store

let () = Random.self_init ()

let maildir =
  Maildir.create
    ~pid:(Unix.getpid ())
    ~host:(Unix.gethostname ())
    ~random:(fun () -> Random.int 1000)
    Store.root

let populate store =
  let mtime = gettimeofday () in
  Hashtbl.add store Fpath.(Store.root / "cur" / "") { mtime; contents= `Directory } ;
  Hashtbl.add store Fpath.(Store.root / "tmp" / "") { mtime; contents= `Directory } ;
  Hashtbl.add store Fpath.(Store.root / "new" / "") { mtime; contents= `Directory } ;
  Alcotest.(check bool) "fs well formed" true (Store.verify (to_fs store) maildir)

let add_one_from_ex_nihilo store =
  let fs = to_fs store in
  let mtime = gettimeofday () in
  let () = Hashtbl.add store ex_nihilo { mtime; contents = `Contents "#message"; } in
  match Store.add fs maildir ~time:(gettimeofday ()) (fun target fs -> Store.transmit fs ex_nihilo target) with
  | Error (`Msg err) -> Alcotest.failf "Store.add fails with: %s" err
  | Ok () ->
      let () = Hashtbl.remove store ex_nihilo in
      let news = Store.scan_only_new (fun news message -> message :: news) [] fs maildir in
      let news = List.fold_left (fun a x -> if Maildir.is_new x then x :: a else a) [] news in
      match news with
      | [] -> Alcotest.failf "Add fails"
      | [ message ] ->
          let key = Maildir.to_fpath maildir message in
          let value = Hashtbl.find store key in
          Alcotest.(check int64) "mtime" value.mtime mtime
      | news ->
          Alcotest.failf "Too much new messages (%d new messages): @[<hov>%a@]"
            (List.length news)
            Fmt.(Dump.list Maildir.pp_message) news

let list_of_len n =
  let rec go acc = function
    | 0 -> acc
    | n -> go (n :: acc) (pred n) in
  if n < 0 then Fmt.invalid_arg "list_of_len"
  else go [] n

let cons a x = x :: a
let only_new a x = if Maildir.is_new x then x :: a else a

let add_some_from_ex_nihilo store n =
  let fs = to_fs store in
  let results =
    List.map (fun _ ->
        let mtime = gettimeofday () in
        Hashtbl.add store ex_nihilo { mtime; contents = `Contents "#message" } ;
        let res = Store.add fs maildir ~time:(gettimeofday ()) (fun target fs -> Store.transmit fs ex_nihilo target) in
        Hashtbl.remove store ex_nihilo ; res)
      (list_of_len n) in
  let () = Alcotest.(check bool) "append" (List.for_all (function Ok () -> true | _ -> false) results) true in
  let news = Store.scan_only_new cons [] fs maildir in
  let news = List.fold_left (fun a x -> if Maildir.is_new x then x :: a else a) [] news in
  Alcotest.(check int) "number of new messages" n (List.length news)

let commit_new_message store =
  let fs = to_fs store in
  match Store.fold only_new [] fs maildir with
  | [] ->
      Alcotest.failf "Commit fails: no new messages"
  | [ message ] ->
      Store.commit fs maildir message ;
      let news = Store.fold only_new [] fs maildir in
      if List.length news <> 0
      then Alcotest.failf "Commit fails, new messages: %a." (Fmt.Dump.list Maildir.pp_message) news
      else ()
  | news -> Alcotest.failf "Too much news messages (%d new messages): @[<hov>%a@]"
              (List.length news)
              Fmt.(Dump.list Maildir.pp_message) news

let take n lst =
  let rec go acc lst n = match n, lst with
    | 0, _ -> List.rev acc
    | n, x :: r -> go (x :: acc) r (pred n)
    | _, [] -> Fmt.invalid_arg "take" in
  if n < 0 then Fmt.invalid_arg "take" else go [] lst n

let commit_some store n =
  let fs = to_fs store in
  match Store.fold only_new [] fs maildir with
  | [] -> Alcotest.failf "Commit some fails: no new messages"
  | news ->
      if List.length news < n
      then Alcotest.failf "Commit some fails: \
                           number of new messages diverge (%d < %d)" (List.length news) n ;

      List.iter (Store.commit fs maildir) (take n news) ;
      let news' = Store.fold only_new [] fs maildir in
      if List.length news - List.length news' <> n
      then Alcotest.failf "Commit fails: committed:%d" (List.length news - List.length news')
      else ()

let test_populate () = Alcotest.test_case "populate" `Quick (fun () -> populate store)
let test_add_one () = Alcotest.test_case "add" `Quick (fun () -> add_one_from_ex_nihilo store)
let test_add_some n = Alcotest.test_case (Fmt.strf "add some (%d)" n) `Quick (fun () -> add_some_from_ex_nihilo store n)
let test_commit () = Alcotest.test_case "commit" `Quick (fun () -> commit_new_message store)
let test_commit_some n = Alcotest.test_case (Fmt.strf "commit some (%d)" n) `Quick (fun () -> commit_some store n)

let () =
  Alcotest.run "maildir-mem"
    [ "populate",
      [ test_populate () ]
    ; "add & commit",
      [ test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit () ]
    ; "add some & commit some",
      [ test_add_some 2
      ; test_commit_some 2
      ; test_add_some 2
      ; test_commit_some 1
      ; test_commit_some 1 ] ]
