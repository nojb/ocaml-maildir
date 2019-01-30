type fs

val fs : fs

type ('a, 'b) transmit = fs -> ('a, 'b) result
(** Type of transmit process. *)

val transmit : fs -> Fpath.t -> Fpath.t -> (unit, Rresult.R.msg) result
(** [transmit a b] creates a process which transmits contents of [a] to [b]. *)

val add : fs -> Maildir.t -> time:int64 -> (Fpath.t -> ('ok, 'err) transmit) -> ('ok, 'err) result
(** [add fs t ~time transmit] adds a new message to Maildir folders [t].
    [transmit] is the process to transmit contents of message to [tmp] folder.
    At the end of [transmit] process, [message] is moved to [new] folder as a
    new message (atomic operation). *)

val scan_only_new : ('a -> Maildir.message -> 'a) -> 'a -> fs -> Maildir.t -> 'a
(** [scan_only_new process acc fs t] scans only new messages in [t]. *)

val fold : ('a -> Maildir.message -> 'a) -> 'a -> fs -> Maildir.t -> 'a
(** [fold process acc fs t] scans messages [cur]rent and [new] messages in [t]. *)

val get : Maildir.t -> Maildir.message -> Fpath.t
(** [get t message] returns location of [message] in [t]. *)

val remove : fs -> Maildir.t -> Maildir.message -> unit
(** [remove fs t message] removes [message] from [t] and [fs]. *)

val get_flags : fs -> Maildir.t -> Maildir.message -> Maildir.flag list
(** [get_flags fs t message] returns flags of [message] available in [t] and [fs]. *)

val set_flags : fs -> Maildir.t -> Maildir.message -> Maildir.flag list -> unit
(** [set_flags fs t messages flags] sets flags of [message] in [t] and [fs] to [flags]. *)
