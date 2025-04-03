val default_timezone : (unit -> Timedesc.Time_zone.t) ref
(** Default timezone to use for date operations. Defaults to the local timezone
    of the system, falling back to UTC if local timezone cannot be determined.
*)

val timedesc_to_ptime : Timedesc.t -> Ptime.t
(** Convert a Timedesc.t to a Ptime.t. *)

val ptime_to_timedesc : ?tz:Timedesc.Time_zone.t -> Ptime.t -> Timedesc.t
(** Convert a Ptime.t to a Timedesc.t with the specified timezone. If no
    timezone is provided, uses the default_timezone. *)

val get_today : (?tz:Timedesc.Time_zone.t -> unit -> Ptime.t) ref
(** Get the current date at midnight in the specified timezone. If no timezone
    is provided, uses the default_timezone. This is a reference to support
    testing. Returns the date or raises an exception if the date cannot be
    determined. *)

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

val get_start_of_current_week : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the start of the current week in the specified timezone. If no timezone
    is provided, uses the default_timezone. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_next_week : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the start of next week in the specified timezone. If no timezone is
    provided, uses the default_timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_week : Ptime.t -> Ptime.t
(** Get the end of the week (Monday) for the given date. Raises an exception if
    the date cannot be calculated. *)

val get_end_of_current_week : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the end of the current week in the specified timezone. If no timezone is
    provided, uses the default_timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_next_week : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the end of next week in the specified timezone. If no timezone is
    provided, uses the default_timezone. Raises an exception if the date cannot
    be calculated. *)

val get_start_of_month : Ptime.t -> Ptime.t
(** Get the start of the month for the given date. Raises an exception if the
    date cannot be calculated. *)

val get_start_of_current_month : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the start of the current month in the specified timezone. If no timezone
    is provided, uses the default_timezone. Raises an exception if the date
    cannot be calculated. *)

val get_start_of_next_month : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the start of next month in the specified timezone. If no timezone is
    provided, uses the default_timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_current_month : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the end of the current month in the specified timezone. If no timezone
    is provided, uses the default_timezone. Raises an exception if the date
    cannot be calculated. *)

val get_end_of_next_month : ?tz:Timedesc.Time_zone.t -> unit -> Ptime.t
(** Get the end of next month in the specified timezone. If no timezone is
    provided, uses the default_timezone. Raises an exception if the date cannot
    be calculated. *)

val get_end_of_month : Ptime.t -> Ptime.t
(** Get the end of the month for the given date. Raises an exception if the date
    cannot be calculated. *)

val convert_relative_date_formats :
  ?tz:Timedesc.Time_zone.t ->
  today:bool ->
  tomorrow:bool ->
  week:bool ->
  month:bool ->
  unit ->
  (Ptime.t * Ptime.t) option
(** Converts relative date formats to determine from/to dates in the specified
    timezone. If no timezone is provided, uses the default_timezone. Returns a
    tuple of (start_date, end_date) or raises an exception if the dates could
    not be determined. **)

val parse_date :
  ?tz:Timedesc.Time_zone.t ->
  string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
(** Parse a date string that could be ISO format (YYYY-MM-DD) or a relative
    expression in the specified timezone. If no timezone is provided, uses the
    default_timezone.

    Supported formats:
    - ISO format:
    - "YYYY-MM-DD" (full date)
    - "YYYY-MM" (partial date)
    - For --from: defaults to first day of month
    - For --to: defaults to last day of month
    - "YYYY" (partial date)
    - For --from: defaults to January 1st of year
    - For --to: defaults to December 31st of year
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

val parse_time : string -> (int * int * int, [> `Msg of string ]) result
(** Parse a time string in HH:MM or HH:MM:SS format. Returns Ok with (hour,
    minute, second) or Error with a message. **)

val parse_date_time :
  ?tz:Timedesc.Time_zone.t ->
  date:string ->
  time:string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
(** Parse a date and time string in the specified timezone. If no timezone is
    provided, uses the default_timezone. *)

val ptime_of_ical : Icalendar.date_or_datetime -> Ptime.t
