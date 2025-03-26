(** Filter-based searching and querying of calendar events *)

type filter
(** Type representing a query filter *)

type sort_order = [ `Ascending | `Descending ]
(** Type representing the sort order *)

type sort_by = [ `Start | `End | `Summary | `Location | `Calendar ]
(** Type representing sort criteria *)

(** {1 Date helper functions} *)

val get_today : (unit -> Ptime.t) ref
(** Get the current date at midnight. This is a reference to support testing.
    Returns the date or raises an exception if the date cannot be determined. *)

val to_end_of_day : Ptime.t -> Ptime.t
(** Converts a date with midnight time (00:00:00) to the same date with
    end-of-day time (23:59:59). This is particularly useful for making "to"
    dates in a range inclusive, addressing the common UX expectation that
    specifying "--to 2025-04-01" would include events occurring on April 1st. *)

val add_days : Ptime.t -> int -> Ptime.t
(** Add specified number of days to a date. Raises an exception if the date
    cannot be calculated. *)

val add_weeks : Ptime.t -> int -> Ptime.t
(** Add specified number of weeks to a date. Raises an exception if the date
    cannot be calculated. *)

val add_months : Ptime.t -> int -> Ptime.t
(** Add specified number of months to a date. Raises an exception if the date
    cannot be calculated. *)

val add_years : Ptime.t -> int -> Ptime.t
(** Add specified number of months to a date. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_week : Ptime.t -> Ptime.t
(** Get the start of the week (Monday) for the given date. Raises an exception
    if the date cannot be calculated. *)

val get_start_of_current_week : unit -> Ptime.t
(** Get the start of the current week. Raises an exception if the date cannot be
    calculated. *)

val get_start_of_next_week : unit -> Ptime.t
(** Get the start of next week. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_week : Ptime.t -> Ptime.t
(** Get the end of the week (Monday) for the given date. Raises an exception if
    the date cannot be calculated. *)

val get_end_of_current_week : unit -> Ptime.t
(** Get the end of the current week. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_next_week : unit -> Ptime.t
(** Get the end of next week. Raises an exception if the date cannot be
    calculated. *)

val get_start_of_month : Ptime.t -> Ptime.t
(** Get the start of the month for the given date. Raises an exception if the
    date cannot be calculated. *)

val get_start_of_current_month : unit -> Ptime.t
(** Get the start of the current month. Raises an exception if the date cannot
    be calculated. *)

val get_start_of_next_month : unit -> Ptime.t
(** Get the start of next month. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_current_month : unit -> Ptime.t
(** Get the end of the current month. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_next_month : unit -> Ptime.t
(** Get the end of next month. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_month : Ptime.t -> Ptime.t
(** Get the end of the month for the given date. Raises an exception if the date
    cannot be calculated. *)

val parse_date_expression : string -> [ `To | `From ] -> Ptime.t
(** Parse a date string that could be ISO format (YYYY-MM-DD) or a relative
    expression. Raises an exception if the date cannot be parsed.

    Supported formats:
    - ISO format: "YYYY-MM-DD"
    - Relative expressions:
    - "today" - Current day
    - "tomorrow" - Next day
    - "yesterday" - Previous day
    - "this-week" - Start of current week
    - "next-week" - Start of next week
    - "this-month" - Start of current month
    - "next-month" - Start of next month
    - "+Nd" - N days from today (e.g., "+7d" for a week from today)
    - "-Nd" - N days before today (e.g., "-7d" for a week ago)
    - "+Nw" - N weeks from today
    - "+Nm" - N months from today *)

val convert_relative_date_formats :
  today:bool ->
  tomorrow:bool ->
  week:bool ->
  month:bool ->
  (Ptime.t * Ptime.t) option
(** Converts relative date formats to determine from/to_ dates. Returns a tuple
    of (start_date, end_date) or raises an exception if the dates could not be
    determined. **)

(** {1 Filter creation} *)

val summary_contains : string -> filter
val description_contains : string -> filter
val location_contains : string -> filter
val in_collections : Calendar_dir.collection list -> filter
val recurring_only : unit -> filter
val non_recurring_only : unit -> filter
val with_id : Event.event_id -> filter

val and_filter : filter list -> filter
(** Filter composition *)

val or_filter : filter list -> filter
val not_filter : filter -> filter

val query :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  Calendar_dir.calendar_dir ->
  ?filter:filter ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  ?sort_by:sort_by ->
  ?order:sort_order ->
  ?limit:int ->
  unit ->
  (Recur.instance list, [> `Msg of string ]) result
(** Find events with expansion of recurring events. Returns Ok with the list of
    instances, or Error with a message. *)

(* Test-only helper functions *)
val matches_filter : Event.t -> filter -> bool
(** Check if an event matches the given filter *)

val compare_events : sort_by -> sort_order -> Event.t -> Event.t -> int
(** Compare two events based on the sort criteria and order *)
