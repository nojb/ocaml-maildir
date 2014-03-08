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

(** Library to access mailbox folders in Maildir format *)

(** The type of Maildir folders. *)
type t

(** The type of message unique identifiers *)
type uid =
  string

(** Message flags *)
type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

val create : ?init:bool -> string -> t
(** [create init path] returns an object that can be used to access a
    Maildir-directory at [path].  If ?init is [true], then the directory [path]
    and its subdirectories "tmp", "cur", and "new" will be created if they do
    not exist.  The default is [false]. *)
  
val update : t -> unit
(** [update md] makes the updates the cached information about the actual contents on-disk. *)
  
val message_add : t -> string -> uid
(** [message_add md data] adds the message with contents [data].  Returns the
    uid of the newly inserted message. *)
  
val message_get : t -> uid -> string
(** [message_get md uid] retrieves the message with uid [uid].

    Raises [Not_found] if no such message is found. *)
  
val message_remove : t -> uid -> unit
(** [message_remove md uid] removes the message with uid [uid].

    Raises [Not_found] if no such message is found. *)
  
val message_change_flags : t -> uid -> flag list -> unit
(** [message_change_flags md uid flags] changes sets the flags of the message
    with uid [uid] to [flags].

    Raises [Not_found] if no such message is found. *)

val message_flags : t -> uid -> flag list
(** [message_flags md uid] returns the list of flags of message with uid [uid].
    
    Raises [Not_found] if no such message is found. *)

val message_iter : (uid -> unit) -> t -> unit
(** [message_iter f md] calls [f] with the uid of each message in [md] in some
    unspecified order. *)

val message_fold : (uid -> 'a -> 'a) -> t -> 'a -> 'a
(** [message_fold f md x] computes [(f uid1 (f uid2 (... (f uidN x))))] where
    [uid1 ... uidN] are the uids of the messages in [md]. *)
