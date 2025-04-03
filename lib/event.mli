(** Core event functionality and data access *)

type event_id = string
(** Event ID type *)

type t
(** Event type representing a calendar event *)

type date_error = [ `Msg of string ]

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  calendar_dir_path:string ->
  summary:string ->
  start:Icalendar.date_or_datetime ->
  ?end_:
    [ `Duration of Icalendar.params * Ptime.Span.t
    | `Dtend of Icalendar.params * Icalendar.date_or_datetime ] ->
  ?location:string ->
  ?description:string ->
  ?recurrence:Icalendar.recurrence ->
  string ->
  t
(** Create a new event with required properties.

    The start and end times can be specified as Icalendar.timestamp values,
    which allows for directly using any of the three RFC5545 time formats:
    - `Utc time: Fixed to absolute UTC time
    - `Local time: Floating local time (follows user's timezone)
    - `With_tzid (time, timezone): Local time with timezone reference *)

val edit :
  ?summary:string ->
  ?start:Icalendar.date_or_datetime ->
  ?end_:
    [ `Duration of Icalendar.params * Ptime.Span.t
    | `Dtend of Icalendar.params * Icalendar.date_or_datetime ] ->
  ?location:string ->
  ?description:string ->
  ?recurrence:Icalendar.recurrence ->
  t ->
  t
(** Edit an existing event. *)

val events_of_icalendar :
  string -> file:Eio.Fs.dir_ty Eio.Path.t -> Icalendar.calendar -> t list

val to_ical_event : t -> Icalendar.event
val to_ical_calendar : t -> Icalendar.calendar
val get_id : t -> event_id
val get_summary : t -> string option

val get_start : t -> Ptime.t
(** Get the start time of an event. Note that local times are converted to UTC
    based on their timezone information. If no timezone is specified, the system
    timezone is used. *)

val get_end : t -> Ptime.t option
(** Get the end time of an event. Like get_start, times are converted to UTC
    based on timezone information. Returns None if the event doesn't have an end
    time. *)

val is_date : t -> bool
(** Returns true if either the start or end timestamp is specified as a date
    instead of a datetime. *)

val get_start_timezone : t -> string option
val get_end_timezone : t -> string option
val get_duration : t -> Ptime.span option
val get_location : t -> string option
val get_description : t -> string option
val get_recurrence : t -> Icalendar.recurrence option
val get_calendar_name : t -> string
val get_file : t -> Eio.Fs.dir_ty Eio.Path.t

type comparator = t -> t -> int
(** Event comparator function type *)

val by_start : comparator
(** Compare events by start time, earlier times come first *)

val by_end : comparator
(** Compare events by end time, earlier times come first. Events with end times
    come after those without *)

val by_summary : comparator
(** Compare events by summary alphabetically. Events with summaries come before
    those without *)

val by_location : comparator
(** Compare events by location alphabetically. Events with locations come before
    those without *)

val by_calendar_name : comparator
(** Compare events by calendar_name name alphabetically *)

val descending : comparator -> comparator
(** Reverse the order of a comparator *)

val chain : comparator -> comparator -> comparator
(** Chain two comparators together, using the second one as a tiebreaker when
    the first one returns equality (0) *)

(** Functions for formatting various data structures as strings *)

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]
(** Format type for output *)

(** Functions for formatting specific event types *)
val format_event : ?format:format -> ?tz:Timedesc.Time_zone.t -> t -> string
(** Format a single event, optionally using the specified timezone *)

val format_events :
  ?format:format -> ?tz:Timedesc.Time_zone.t -> t list -> string
(** Format a list of events, optionally using the specified timezone *)

val expand_recurrences : from:Ptime.t option -> to_:Ptime.t -> t -> t list
