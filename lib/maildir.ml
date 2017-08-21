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

let () =
  Random.self_init ()

type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

let char_of_flag = function
  | SEEN -> Some 'S'
  | REPLIED -> Some 'R'
  | FLAGGED -> Some 'F'
  | TRASHED -> Some 'T'
  | PASSED -> Some 'P'
  | DRAFT -> Some 'D'
  | NEW -> None

let flag_of_char = function
  | 'S' -> Some SEEN
  | 'R' -> Some REPLIED
  | 'F' -> Some FLAGGED
  | 'T' -> Some TRASHED
  | 'P' -> Some PASSED
  | 'D' -> Some DRAFT
  | _ -> None

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

type uid =
  string

type msg =
  {
    uid: uid;
    filename: string;
    flags: flag list;
  }

type t =
  {
    pid: int;
    hostname: string;
    path: string;
    mutable counter: int;
    mutable mtime_new: float;
    mutable mtime_cur: float;
    msg_hash: (string, msg) Hashtbl.t;
  }

exception Message_not_found of string

let default_hash_size = 128

let create_if_needed path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o700

let strstr s1 s2 =
  let rec loop i =
    if i + String.length s2 > String.length s1 then
      String.length s1
    else
      let rec matches j =
        if j >= String.length s2 then true
        else if s1.[i+j] = s2.[j] then matches (j+1) else false
      in
      if matches 0 then i else loop (i+1)
  in
  loop 0

let msg_new filename is_new =
  let rec pflags flags p =
    if p < String.length filename then
      match flag_of_char filename.[p] with
      | None -> pflags flags (p+1)
      | Some f -> pflags (f :: flags) (p+1)
    else
      flags
  in
  let p = strstr filename ":2," in
  let flags = pflags [] (p+3) in
  let flags = if is_new then NEW :: flags else flags in
  {uid = String.sub filename 0 p; flags; filename}

let add_message m filename is_new =
  let msg = msg_new filename is_new in
  Hashtbl.add m.msg_hash msg.uid msg

let iter_dir path f =
  let d = Unix.opendir path in
  let rec loop () =
    match Unix.readdir d with
    | entry ->
        f entry;
        loop ()
    | exception End_of_file ->
        Unix.closedir d
    | exception e ->
        Unix.closedir d;
        raise e
  in
  loop ()

let add_directory m path is_new =
  iter_dir path (fun entry ->
      if entry.[0] <> '.' then
        try add_message m entry is_new with _ -> ()
    )

let flush m =
  Hashtbl.clear m.msg_hash

let get_new_message_filename ?(date = Unix.gettimeofday ()) m =
  let basename =
    Printf.sprintf "%.0f.R%xP%dQ%d.%s" (date *. 1_000_000.)
      (Random.bits ()) m.pid m.counter m.hostname
  in
  let filename =
    Printf.sprintf "%s/tmp/%s" m.path basename
  in
  m.counter <- m.counter + 1;
  assert (not (Sys.file_exists filename));
  filename

let update m =
  let path_new = Filename.concat m.path "new" in
  let path_cur = Filename.concat m.path "cur" in
  let stat_info = Unix.stat path_new in
  let new_changed =
    if stat_info.Unix.st_mtime <> m.mtime_new then
      (m.mtime_new <- stat_info.Unix.st_mtime; true)
    else
      false
  in
  let stat_info = Unix.stat path_cur in
  let cur_changed =
    if stat_info.Unix.st_mtime <> m.mtime_cur then
      (m.mtime_cur <- stat_info.Unix.st_mtime; true)
    else
      false
  in
  if new_changed || cur_changed then begin
    flush m;
    add_directory m path_new true;
    add_directory m path_cur false
  end

let sanitize s =
  let b = Buffer.create (String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '/' -> Buffer.add_string b "\\057"
    | ':' -> Buffer.add_string b "\\072"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let create path =
  let hostname = sanitize (Unix.gethostname ()) in
  let pid = Unix.getpid () in
  let counter = 0 in
  let mtime_new = -1. in
  let mtime_cur = -1. in
  let msg_hash = Hashtbl.create default_hash_size in
  let m = {path; pid; hostname; counter; mtime_new; mtime_cur; msg_hash} in
  create_if_needed path;
  create_if_needed (Filename.concat path "tmp");
  create_if_needed (Filename.concat path "new");
  create_if_needed (Filename.concat path "cur");
  update m;
  m

let with_open_out_bin path f =
  let oc = open_out_bin path in
  match f oc with
  | r ->
      close_out oc;
      r
  | exception e ->
      close_out_noerr oc;
      raise e

let add m ?date message =
  update m;
  let tmp_name = get_new_message_filename ?date m in
  with_open_out_bin tmp_name (fun oc -> output_string oc message);
  let tmp_basename = Filename.basename tmp_name in
  let new_name = Filename.concat (Filename.concat m.path "new") tmp_basename in
  Unix.link tmp_name new_name;
  Unix.unlink tmp_name;
  let path_new = Filename.concat m.path "new" in
  let stat_info = Unix.stat path_new in
  m.mtime_new <- stat_info.Unix.st_mtime;
  let new_basename = Filename.basename new_name in
  match add_message m new_basename true with
  | () ->
      new_basename
  | exception e ->
      begin try Unix.unlink new_name with _ -> () end;
      raise e

let get m uid =
  match Hashtbl.find m.msg_hash uid with
  | msg ->
      let dir = if List.mem NEW msg.flags then "new" else "cur" in
      Filename.concat (Filename.concat m.path dir) msg.filename
  | exception Not_found ->
      raise (Message_not_found uid)

let remove m uid =
  match Hashtbl.find m.msg_hash uid with
  | msg ->
      let dir = if List.mem NEW msg.flags then "new" else "cur" in
      let filename = Filename.concat (Filename.concat m.path dir) msg.filename in
      Unix.unlink filename;
      Hashtbl.remove m.msg_hash uid
  | exception Not_found ->
      raise (Message_not_found uid)

let set_flags m uid new_flags =
  match Hashtbl.find m.msg_hash uid with
  | msg ->
      let dir = if List.mem NEW msg.flags then "new" else "cur" in
      let filename = Filename.concat (Filename.concat m.path dir) msg.filename in
      let dir = if List.mem NEW new_flags then "new" else "cur" in
      let flag_str = string_of_flags new_flags in
      let new_filename =
        if String.length flag_str = 0 then
          Filename.concat (Filename.concat m.path dir) msg.uid
        else
          Filename.concat (Filename.concat m.path dir)
            (Printf.sprintf "%s:2,%s" msg.uid flag_str)
      in
      if filename <> new_filename then begin
        Unix.link filename new_filename;
        Unix.unlink filename;
        Hashtbl.replace m.msg_hash uid
          { msg with
            filename = Filename.basename new_filename;
            flags = new_flags }
      end
  | exception Not_found ->
      raise (Message_not_found uid)

let get_flags m uid =
  match Hashtbl.find m.msg_hash uid with
  | msg ->
      msg.flags
  | exception Not_found ->
      raise (Message_not_found uid)

let iter f m =
  Hashtbl.iter (fun _ msg -> f msg) m.msg_hash

let fold f m x =
  Hashtbl.fold (fun _ msg x -> f msg x) m.msg_hash x
