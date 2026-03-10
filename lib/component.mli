(** Unified component handling for events, todos, and journals *)

type component_type = CEvent | CTodo | CJournal

type t

val component_type : t -> component_type
val of_event : Event.t -> t
val of_todo : Todo.t -> t
val of_journal : Journal.t -> t

val to_event : t -> Event.t option
val to_todo : t -> Todo.t option
val to_journal : t -> Journal.t option

val get_id : t -> string
val get_summary : t -> string option
val get_description : t -> string option
val get_categories : t -> string list
val get_calendar_name : t -> string
val get_file : t -> Eio.Fs.dir_ty Eio.Path.t
val get_alarms : t -> Icalendar.alarm list
val get_start : t -> Ptime.t option

val to_ical_component : t -> Icalendar.component
val to_ical_calendar : t -> Icalendar.calendar

val components_of_icalendar :
  string -> file:Eio.Fs.dir_ty Eio.Path.t -> Icalendar.calendar -> t list

(** Comparators *)
type comparator = t -> t -> int

val by_start : comparator
val by_summary : comparator
val by_calendar_name : comparator
val by_type : comparator
val descending : comparator -> comparator
val chain : comparator -> comparator -> comparator

(** Filters *)
type filter = t -> bool

val is_type : component_type -> filter
val summary_contains : string -> filter
val description_contains : string -> filter
val in_calendars : string list -> filter
val has_categories : string list -> filter
val and_filter : filter list -> filter
val or_filter : filter list -> filter
val not_filter : filter -> filter

(** Formatting *)
type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

val format_component : ?format:format -> ?tz:(unit -> Timedesc.Time_zone.t) -> t -> string
val format_components :
  ?format:format -> ?tz:(unit -> Timedesc.Time_zone.t) -> ?get_color:(string -> string option) -> t list -> string

val sexp_of_t : t -> Sexplib0.Sexp.t

(** Alarm fire times *)

type alarm_fire = {
  fire_time : Ptime.t;
  component : t;
  alarm : Icalendar.alarm;
}

val query_alarm_fires : from:Ptime.t option -> to_:Ptime.t -> t list -> alarm_fire list
(** Query all alarm fire times within a date range, sorted by fire time. *)