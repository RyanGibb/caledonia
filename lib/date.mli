val local_timezone : unit -> Timedesc.Time_zone.t
(** The local timezone of the system, falling back to UTC if the local
    timezone cannot be determined. Used as the default for date operations
    when no timezone is given. *)

val timedesc_to_ptime : Timedesc.t -> Ptime.t
(** Convert a Timedesc.t to a Ptime.t. *)

val ptime_to_timedesc : ?tz:Timedesc.Time_zone.t -> Ptime.t -> Timedesc.t
(** Convert a Ptime.t to a Timedesc.t with the specified timezone. If no
    timezone is provided, uses the local timezone. *)

val get_today :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the current date at midnight in the specified timezone. If no timezone
    is provided, uses the local timezone. [?now] overrides the current instant
    (useful for testing). Raises an exception if the date cannot be
    determined. *)

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

val get_start_of_current_week :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of the current week in the specified timezone. If no timezone
    is provided, uses the local timezone. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_next_week :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of next week in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_week : Ptime.t -> Ptime.t
(** Get the exclusive end of the week for the given date: midnight at the
    start of the following Monday. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_current_week :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of the current week in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_next_week :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of next week in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_start_of_month : Ptime.t -> Ptime.t
(** Get the start of the month for the given date. Raises an exception if the
    date cannot be calculated. *)

val get_start_of_current_month :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of the current month in the specified timezone. If no timezone
    is provided, uses the local timezone. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_next_month :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of next month in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_current_month :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of the current month in the specified timezone. If no timezone
    is provided, uses the local timezone. Raises an exception if the date
    cannot be calculated. *)

val get_end_of_next_month :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of next month in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_month : Ptime.t -> Ptime.t
(** Get the exclusive end of the month for the given date: midnight at the
    start of the following month. Raises an exception if the date cannot be
    calculated. *)

val get_start_of_year : Ptime.t -> Ptime.t
(** Get the start of the year (Jan 1) for the given date. Raises an exception if
    the date cannot be calculated. *)

val get_start_of_current_year :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of the current year in the specified timezone. If no timezone
    is provided, uses the local timezone. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_next_year :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the start of next year in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_year : Ptime.t -> Ptime.t
(** Get the exclusive end of the year for the given date: midnight on Jan 1 of
    the following year. Raises an exception if the date cannot be
    calculated. *)

val get_end_of_current_year :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of the current year in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_next_year :
  ?tz:Timedesc.Time_zone.t -> ?now:Ptime.t -> unit -> Ptime.t
(** Get the end of next year in the specified timezone. If no timezone is
    provided, uses the local timezone. Raises an exception if the date cannot
    be calculated. *)

val convert_relative_date_formats :
  ?tz:Timedesc.Time_zone.t ->
  ?now:Ptime.t ->
  today:bool ->
  tomorrow:bool ->
  week:bool ->
  month:bool ->
  unit ->
  (Ptime.t * Ptime.t) option
(** Converts relative date formats to determine from/to dates in the specified
    timezone. If no timezone is provided, uses the local timezone. Returns a
    tuple of (start_date, end_date), where end_date is an exclusive bound
    (midnight after the last included day), or raises an exception if the
    dates could not be determined. **)

val parse_date :
  ?tz:Timedesc.Time_zone.t ->
  ?now:Ptime.t ->
  string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
(** Parse a date string that could be ISO format (YYYY-MM-DD) or a relative
    expression in the specified timezone. If no timezone is provided, uses the
    local timezone.

    All [`To] results are exclusive upper bounds: midnight at the start of the
    day after the named day/week/month/year, so that events on the last named
    day are included when ranges are compared with [start < to].

    Supported formats:
    - ISO format:
    - "YYYY-MM-DD" (full date)
    - "YYYY-MM" (partial date)
    - For --from: first day of the month
    - For --to: exclusive end of the month
    - "YYYY" (partial date)
    - For --from: January 1st of the year
    - For --to: exclusive end of the year
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
    - "+Nm" - N months from today
    - "+Ny" - N years from today *)

val parse_time : string -> (int * int * int, [> `Msg of string ]) result
(** Parse a time string in HH:MM or HH:MM:SS format. Returns Ok with (hour,
    minute, second) or Error with a message. **)

val parse_date_time :
  ?tz:Timedesc.Time_zone.t ->
  ?now:Ptime.t ->
  date:string ->
  time:string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
(** Parse a date and time string in the specified timezone. If no timezone is
    provided, uses the local timezone. *)

val ptime_of_ical : Icalendar.date_or_datetime -> Ptime.t
