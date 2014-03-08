open Maildir
open OUnit2

let read_contents path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = String.create len in
  really_input ic buf 0 len;
  buf

let contents1 =
  "test1"
  
let test ctxt =
  let md = create ~init:true (bracket_tmpdir ctxt) in
  let uid = add md contents1 in
  update md;
  let path = get md uid in
  assert_equal ~ctxt (read_contents path) contents1;
  assert_equal ~ctxt (flags md uid) [NEW];
  set_flags md uid [SEEN; FLAGGED];
  update md;
  let path = get md uid in
  assert_equal ~ctxt (read_contents path) contents1;
  assert_equal ~ctxt (flags md uid) [SEEN; FLAGGED];
  remove md uid;
  assert_raises Not_found (fun () -> get md uid);
  update md;
  assert_raises Not_found (fun () -> get md uid)

let _ =
  run_test_tt_main ("test" >:: test)
