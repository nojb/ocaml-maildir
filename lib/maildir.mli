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

(** Library to access mailbox folders in Maildir format *)

(** The type of Maildir folders. *)
type t

(** Message flags *)
type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

(** The type of message unique identifiers *)
type uniq =
  { sequence : int option
  ; boot : int option
  ; crypto_random : int option
  ; inode : int option
  ; device : int option
  ; microsecond : int option
  ; pid : int option
  ; deliveries : int option }

type uid =
  | Modern of uniq
  | Old0 of int
  | Old1 of int * int

type info =
  | Info of flag list

type message =
  { time : int
  ; uid : uid
  ; info : info
  ; host : string
  ; parameters : (string * string) list }

val is_new : message -> bool
val with_new : message -> message

type filename = string

val to_filename : message -> filename
val of_filename : filename -> (message, Rresult.R.msg) result

val create : pid:int -> host:string -> random:(unit -> int) -> Fpath.t -> t

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

module Make (IO : IO) (FS : FS with type +'a io = 'a IO.t and type key = Fpath.t) : sig
  val add : FS.t -> t -> time:int -> (FS.key -> (unit -> ('a, 'b) result IO.t)) -> ('a, 'b) result IO.t
  val scan_only_new : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  val fold : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  val get : t -> message -> FS.key
  val remove : FS.t -> t -> message -> unit IO.t
  val get_flags : FS.t -> t -> message -> flag list IO.t
  val set_flags : FS.t -> t -> message -> flag list -> unit IO.t
end
