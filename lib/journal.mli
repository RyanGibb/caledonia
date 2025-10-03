(** Journal entry handling *)

type t

val sexp_of_t : t -> Sexplib0.Sexp.t

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  calendar_dir_path:string ->
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  string ->
  (t, [> `Msg of string ]) result

val edit :
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  t ->
  (t, [> `Msg of string ]) result

val journals_of_icalendar :
  string -> file:Eio.Fs.dir_ty Eio.Path.t -> Icalendar.calendar -> t list

val to_ical_journal : t -> Icalendar.journal_prop list
val to_ical_calendar : t -> Icalendar.calendar

val get_id : t -> string
val get_summary : t -> string option
val get_start : t -> Ptime.t option
val get_description : t -> string option
val get_categories : t -> string list
val get_status : t -> Icalendar.status option
val get_calendar_name : t -> string
val get_file : t -> Eio.Fs.dir_ty Eio.Path.t

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

val format_journal : ?format:format -> ?tz:Timedesc.Time_zone.t -> t -> string
val format_journals : ?format:format -> ?tz:Timedesc.Time_zone.t -> ?get_color:(string -> string option) -> t list -> string