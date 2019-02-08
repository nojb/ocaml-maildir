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

(** The type of message modern-unique identifiers. *)
type uniq =
  { sequence : (int64 * raw) option
  ; boot : (int64 * raw) option
  ; crypto_random : (int64 * raw) option
  ; inode : (int64 * raw) option
  ; device : (int64 * raw) option
  ; microsecond : (int64 * raw) option
  ; pid : (int * raw) option
  ; deliveries : (int * raw) option
  ; order : v_uniq_flag list }
and raw = string

(* The type of message unique identifiers. *)
type uid =
  | Modern of uniq
  | Old0 of int
  | Old1 of int * int

val pp_uid : uid Fmt.t
(** Pretty-printer of {!uid}. *)

(** The type of message informations. *)
type info =
  | Info of flag list

val pp_info : info Fmt.t
(** Pretty-printer of {!info}. *)

type message =
  { time : int64
  ; uid : uid
  ; info : info
  ; host : string
  ; parameters : (string * string) list }

val pp_message : message Fmt.t
(** Prettry-printer of {!message}. *)

val equal_uniq : uniq -> uniq -> bool
val equal_flag : flag -> flag -> bool
val equal_info : info -> info -> bool
val equal_uid : uid -> uid -> bool
val equal_parameters : (string * string) list -> (string * string) list -> bool
val equal_message : message -> message -> bool

val is_new : message -> bool
(** [is_new message] returns [true] if [message] has the flag {!NEW}. *)

val with_new : message -> message
(** [with_new message] returns a new message {i flagged} with {!NEW}. *)

(** Type of filename. *)
type filename = string

val to_filename : message -> filename
(** [to_filename message] returns a {!filename} which corresponds to [message]
    (according to Maildir format). *)

val to_fpath : t -> message -> Fpath.t
(** [to_fpath] returns a [Fpath.t] which corresponds to [message]
    (according to Maildir format). *)

val of_filename : filename -> (message, Rresult.R.msg) result
(** [of_filename filename] tries to parse [filename] and returns unique message
   identifier. *)

val create : pid:int -> host:string -> random:(unit -> int64) -> Fpath.t -> t
(** [create ~pid ~host ~random path] returns a witness of Maildir folders at
    [path]. *)

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
  type ('a, 'b) transmit = FS.t -> ('a, 'b) result IO.t
  (** Type of transmit process. *)

  val verify : FS.t -> t -> bool IO.t

  val add : FS.t -> t -> time:int64 -> (FS.key -> ('ok, 'err) transmit) -> ('ok, 'err) result IO.t
  (** [add fs t ~time transmit] adds a new message to Maildir folders [t].
      [transmit] is the process to transmit contents of message to [tmp] folder.
      At the end of [transmit] process, [message] is moved to [new] folder as a
      new message (atomic operation). *)

  val scan_only_new : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  (** [scan_only_new process acc fs t] scans only new messages in [t]. *)

  val fold : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  (** [fold process acc fs t] scans messages [cur]rent and [new] messages in [t]. *)

  val get : t -> message -> FS.key
  (** [get t message] returns location of [message] in [t]. *)

  val commit : FS.t -> t -> ?flags:flag list -> message -> unit IO.t
  (** [commit fs t message] commits new [message] to "cur" directory. *)

  val remove : FS.t -> t -> message -> unit IO.t
  (** [remove fs t message] removes [message] from [t] and [fs]. *)

  val get_flags : FS.t -> t -> message -> flag list IO.t
  (** [get_flags fs t message] returns flags of [message] available in [t] and [fs]. *)

  val set_flags : FS.t -> t -> message -> flag list -> unit IO.t
  (** [set_flags fs t messages flags] sets flags of [message] in [t] and [fs] to [flags]. *)
end
