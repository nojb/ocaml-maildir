(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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
  let l = List.fold_right (fun f l ->
      match char_of_flag f with
      | Some c -> c :: l | None -> l) flags []
  in
  let l = List.sort compare l in
  let b = Buffer.create (List.length l) in
  let rec loop prev l =
    match prev, l with
    | _, [] ->
      Buffer.contents b
    | None, c :: rest ->
      Buffer.add_char b c;
      loop (Some c) rest
    | Some c, c' :: rest ->
      if c = c' then
        loop (Some c) rest
      else begin
        Buffer.add_char b c';
        loop (Some c') rest
      end
  in
  loop None l

type uid =
  string

type msg = {
  uid : uid;
  filename : string;
  flags : flag list
}

type t = {
  pid : int;
  hostname : string;
  path : string;
  mutable counter : int;
  mutable mtime_new : float;
  mutable mtime_cur : float;
  msg_hash : (string, msg) Hashtbl.t
}

let default_hash_size = 128

let exists path =
  try
    ignore (Unix.stat path);
    true
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    false

let create_if_needed path =
  if not (exists path) then
    Unix.mkdir path 0o700

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
  { uid = String.sub filename 0 p; flags; filename }

let add_message md filename is_new =
  let msg = msg_new filename is_new in
  Hashtbl.add md.msg_hash msg.uid msg

let iter_dir path f =
  let d = Unix.opendir path in
  try
    let rec loop () =
      let entry = Unix.readdir d in
      f entry;
      loop ()
    in
    loop ()
  with
  | End_of_file ->
    Unix.closedir d
  | exn ->
    Unix.closedir d;
    raise exn

let add_directory md path is_new =
  iter_dir path
    (fun entry ->
       if entry.[0] <> '.' then
         try add_message md entry is_new with _ -> ())

let flush md =
  Hashtbl.clear md.msg_hash

let max_try_alloc = 32

let get_new_message_filename md =
  let now = Unix.time () in
  let rec loop i =
    if i >= max_try_alloc then raise Not_found
    else
      let basename =
        Printf.sprintf "%.0f.%d_%d.%s" now md.pid md.counter md.hostname
      in
      let filename =
        Printf.sprintf "%s/tmp/%s" md.path basename
      in
      try
        md.counter <- md.counter + 1;
        ignore (Unix.stat filename);
        raise Exit
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
        filename
      | exn ->
        loop (i+1)
  in
  loop 0

let update md =
  let path_new = Printf.sprintf "%s/new" md.path in
  let path_cur = Printf.sprintf "%s/cur" md.path in
  let stat_info = Unix.stat path_new in
  let new_changed =
    if stat_info.Unix.st_mtime <> md.mtime_new then
      (md.mtime_new <- stat_info.Unix.st_mtime; true)
    else
      false
  in
  let stat_info = Unix.stat path_cur in
  let cur_changed =
    if stat_info.Unix.st_mtime <> md.mtime_cur then
      (md.mtime_cur <- stat_info.Unix.st_mtime; true)
    else
      false
  in
  if new_changed || cur_changed then begin
    flush md;
    add_directory md path_new true;
    add_directory md path_cur false
  end
  
let create ?init:(init=false) path =
  let hostname = Unix.gethostname () in
  let pid = Unix.getpid () in
  let counter = 0 in
  let mtime_new = -1. in
  let mtime_cur = -1. in
  let msg_hash = Hashtbl.create default_hash_size in
  let md =
    { path; pid; hostname; counter; mtime_new;
      mtime_cur; msg_hash }
  in
  if init then begin
    create_if_needed path;
    create_if_needed (Printf.sprintf "%s/tmp" path);
    create_if_needed (Printf.sprintf "%s/new" path);
    create_if_needed (Printf.sprintf "%s/cur" path)
  end;
  update md;
  md

let add md message =
  update md;
  let tmp_name = get_new_message_filename md in
  let oc = open_out_bin tmp_name in
  output_string oc message;
  close_out oc;
  let tmp_basename = Filename.basename tmp_name in
  let new_name =
    Printf.sprintf "%s/new/%s" md.path tmp_basename
  in
  Unix.link tmp_name new_name;
  Unix.unlink tmp_name;
  let path_new = Printf.sprintf "%s/new" md.path in
  let stat_info = Unix.stat path_new in
  md.mtime_new <- stat_info.Unix.st_mtime;
  let new_basename = Filename.basename new_name in
  try
    add_message md new_basename true;
    new_basename
  with
  | exn ->
    begin try Unix.unlink new_name with _ -> () end;
    raise exn

let get md uid =
  let msg = Hashtbl.find md.msg_hash uid in
  let dir =
    if List.mem NEW msg.flags then "new" else "cur"
  in
  Printf.sprintf "%s/%s/%s" md.path dir msg.filename

let remove md uid =
  let msg = Hashtbl.find md.msg_hash uid in
  let dir =
    if List.mem NEW msg.flags then "new" else "cur"
  in
  let filename =
    Printf.sprintf "%s/%s/%s" md.path dir msg.filename
  in
  Unix.unlink filename;
  Hashtbl.remove md.msg_hash uid

let set_flags md uid new_flags =
  let msg = Hashtbl.find md.msg_hash uid in
  let dir =
    if List.mem NEW msg.flags then "new" else "cur"
  in
  let filename =
    Printf.sprintf "%s/%s/%s" md.path dir msg.filename
  in
  let dir =
    if List.mem NEW new_flags then "new" else "cur"
  in
  let flag_str = string_of_flags new_flags in
  let new_filename =
    if String.length flag_str = 0 then
      Printf.sprintf "%s/%s/%s" md.path dir msg.uid
    else
      Printf.sprintf "%s/%s/%s:2,%s" md.path dir msg.uid flag_str
  in
  if filename <> new_filename then begin
    Unix.link filename new_filename;
    Unix.unlink filename;
    Hashtbl.replace md.msg_hash uid
      { msg with
        filename = Filename.basename new_filename;
        flags = new_flags }
  end
        
let flags md uid =
  let msg = Hashtbl.find md.msg_hash uid in
  msg.flags

let iter f md =
  Hashtbl.iter (fun uid _ -> f uid) md.msg_hash

let fold f md x =
  Hashtbl.fold (fun uid _ x -> f uid x) md.msg_hash x
