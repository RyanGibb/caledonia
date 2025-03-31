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

val convert_relative_date_formats :
  today:bool ->
  tomorrow:bool ->
  week:bool ->
  month:bool ->
  (Ptime.t * Ptime.t) option
(** Converts relative date formats to determine from/to dates. Returns a tuple
    of (start_date, end_date) or raises an exception if the dates could not be
    determined. **)

val parse_date :
  string -> [ `To | `From ] -> (Ptime.t, [> `Msg of string ]) result
(** Parse a date string that could be ISO format (YYYY-MM-DD) or a relative
    expression.

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

val parse_time : string -> (int * int * int, [> `Msg of string ]) result
(** Parse a time string in HH:MM or HH:MM:SS format. Returns Ok with (hour,
    minute, second) or Error with a message. **)

val parse_date_time :
  date:string ->
  time:string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result

val parse_date_time_opt :
  date:string ->
  ?time:string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
