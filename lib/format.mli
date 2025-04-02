(** Functions for formatting various data structures as strings *)

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]
(** Format type for output *)

(** Functions for formatting specific event types *)
val format_event :
  fs:'a Eio.Path.t ->
  calendar_dir:Calendar_dir.t ->
  ?format:format ->
  ?tz:Timedesc.Time_zone.t ->
  Event.t ->
  string
(** Format a single event, optionally using the specified timezone *)

val format_instance :
  fs:'a Eio.Path.t ->
  calendar_dir:Calendar_dir.t ->
  ?format:format ->
  ?tz:Timedesc.Time_zone.t ->
  Recur.instance ->
  string
(** Format a single event instance, optionally using the specified timezone *)

val format_events :
  fs:'a Eio.Path.t ->
  calendar_dir:Calendar_dir.t ->
  ?format:format ->
  ?tz:Timedesc.Time_zone.t ->
  Event.t list ->
  string
(** Format a list of events, optionally using the specified timezone *)

val format_instances :
  fs:'a Eio.Path.t ->
  calendar_dir:Calendar_dir.t ->
  ?format:format ->
  ?tz:Timedesc.Time_zone.t ->
  Recur.instance list ->
  string
(** Format a list of event instances, optionally using the specified timezone *)
