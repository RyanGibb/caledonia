(** Filter-based searching and querying of calendar events *)

type filter = Event.t -> bool

val summary_contains : string -> filter
val description_contains : string -> filter
val location_contains : string -> filter
val in_collections : Collection.t list -> filter
val recurring_only : unit -> filter
val non_recurring_only : unit -> filter
val with_id : Event.event_id -> filter
val and_filter : filter list -> filter
val or_filter : filter list -> filter
val not_filter : filter -> filter

val query_events :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  Calendar_dir.t ->
  ?filter:filter ->
  ?comparator:Event.comparator ->
  ?limit:int ->
  unit ->
  (Event.t list, [> `Msg of string ]) result
(** Find events without expansion of recurring events. Returns Ok with the list
    of events, or Error with a message. *)

val query :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  Calendar_dir.t ->
  ?filter:filter ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  ?comparator:Recur.Instance.comparator ->
  ?limit:int ->
  unit ->
  (Recur.instance list, [> `Msg of string ]) result
(** Find events with expansion of recurring events. Returns Ok with the list of
    instances, or Error with a message. *)

(* Test-only helper functions *)
val matches_filter : Event.t -> filter -> bool
(** Check if an event matches the given filter *)
