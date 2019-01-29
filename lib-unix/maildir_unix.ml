module IO : Maildir.IO with type +'a t = 'a = struct
  type +'a t = 'a

  let bind v f = f v
  let map f v = f v
  let return v = v

  let (>>=) = bind
  let (>>|) v f = map f v
end

module FS : Maildir.FS
  with type +'a io = 'a IO.t
   and type t = unit
   and type key = Fpath.t = struct
  type key = Fpath.t
  type t = unit

  type +'a io = 'a IO.t

  let mtime () path =
    let unix_path = Fpath.to_string path in
    let stat = Unix.stat unix_path in
    Int64.of_float stat.Unix.st_mtime

  let fold () path computation acc =
    match Bos.OS.Dir.fold_contents
            ~elements:`Files computation acc path with
    | Ok v -> v
    | Error (`Msg err) ->
        Fmt.epr "Retrieve an error when we want to scan %a: %s.\n%!"
          Fpath.pp path err ;
        Fmt.invalid_arg "%s" err

  let rename () a b =
    match Bos.OS.Path.move ~force:true a b with
    | Ok v -> v
    | Error _ -> assert false (* should never occur. *)

  let remove () path =
    match Bos.OS.File.delete path with
    | Ok v -> v
    | Error (`Msg err) ->
        Fmt.epr "Retrieve an error when we remove %a: %s.\n%!"
          Fpath.pp path err

  let exists () path =
    match Bos.OS.File.exists path with
    | Ok v -> v
    | Error (`Msg err) ->
        Fmt.epr "Retrieve an error when we want to access to %a: %s.\n%!"
          Fpath.pp path err ;
        Fmt.invalid_arg "%s" err
end

module Maildir = Maildir.Make (IO) (FS)

type fs = FS.t

let fs : fs = ()

[@@@warning "-37"]

module Never
  : sig type t = private Never end
  = struct type t = Never end

let transmit () a b =
  let input rd buf =
    let rec go () = match rd () with
      | Some (raw, off, len) ->
          Buffer.add_subbytes buf raw off len ; go ()
      | None -> Buffer.contents buf in
    go ()
  in
  let output wr (raw, chunk) =
    let len = String.length raw in
    let rec go pos =
      if pos < len
      then let len' = min (len - pos) chunk in
        wr (Some (Bytes.unsafe_of_string raw, pos, len')) ; go (pos + len')
      else ( wr None ; Ok pos ) in
    go 0
  in
  let buf = Buffer.create 0x800 in

  let open Rresult.R in

  Bos.OS.File.with_input a input buf >>= fun contents ->
  Bos.OS.File.with_output b output (contents, 0x100) >>= function
  | Ok write ->
      Buffer.clear buf ;
      Fmt.epr "Transmit %d byte(s) from %a to %a.\n%!" write Fpath.pp a Fpath.pp b ;
      Ok ()
  | Error Never.Never -> assert false

include Maildir
