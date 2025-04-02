(** Functions for formatting various data structures as strings *)

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]
(** Format type for output *)

(** Functions for formatting specific event types *)
val format_event :
  ?format:format -> ?tz:Timedesc.Time_zone.t -> Event.t -> string
(** Format a single event, optionally using the specified timezone *)

val format_instance :
  ?format:format -> ?tz:Timedesc.Time_zone.t -> Recur.instance -> string
(** Format a single event instance, optionally using the specified timezone *)

val format_events :
  ?format:format -> ?tz:Timedesc.Time_zone.t -> Event.t list -> string
(** Format a list of events, optionally using the specified timezone *)

val format_instances :
  ?format:format -> ?tz:Timedesc.Time_zone.t -> Recur.instance list -> string
(** Format a list of event instances, optionally using the specified timezone *)
