module IO : Maildir.IO with type +'a t = 'a = struct
  type +'a t = 'a

  let bind v f = f v
  let map f v = f v
  let return v = v

  let (>>=) = bind
  let (>>|) v f = map f v
end

module FS = struct
  type key = Fpath.t
  type elt = { mutable mtime : int64; contents : string; }
  type t = (key, elt) Hashtbl.t

  type +'a io = 'a IO.t

  let mtime fs path =
    match Hashtbl.find fs path with
    | { mtime; _ } -> mtime
    | exception Not_found -> Fmt.invalid_arg "%a not found" Fpath.pp path

  let fold fs path computation acc =
    Hashtbl.fold
      (fun k _ a ->
         if Fpath.(equal (base k) path)
         then computation path a
         else a)
      fs acc

  let rename fs a b =
    match Hashtbl.find fs a with
    | v ->
      Hashtbl.remove fs a ; Hashtbl.add fs b v
    | exception Not_found -> Fmt.invalid_arg "%a not found" Fpath.pp a

  let remove fs path = Hashtbl.remove fs path

  let exists fs path =
    try ignore @@ Hashtbl.find fs path ; true with Not_found -> false
end

module Maildir = Maildir.Make (IO) (FS)

type fs = FS.t

let fs length : fs = Hashtbl.create length

let transmit fs a b =
  match Hashtbl.find fs a with
  | v -> Hashtbl.replace fs b v ; Ok ()
  | exception Not_found -> Rresult.R.error_msgf "%a not found" Fpath.pp b

include Maildir
