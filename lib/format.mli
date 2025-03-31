(** Functions for formatting various data structures as strings *)

type format = [ `Text | `Json | `Csv | `Ics | `Entries | `Sexp ]
(** Format type for output *)

(** Functions for formatting specific event types *)
val format_event : ?format:format -> Event.t -> string
(** Format a single event *)

val format_instance : ?format:format -> Recur.instance -> string
(** Format a single event instance *)

val format_events : ?format:format -> Event.t list -> string
(** Format a list of events *)

val format_instances : ?format:format -> Recur.instance list -> string
(** Format a list of event instances *)
