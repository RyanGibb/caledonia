(** Todo task handling *)

type t

val sexp_of_t : t -> Sexplib0.Sexp.t

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  calendar_dir_path:string ->
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?due:Icalendar.params * Icalendar.date_or_datetime ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  ?priority:int ->
  ?percent:int ->
  ?parent:string ->
  string ->
  (t, [> `Msg of string ]) result

val edit :
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?due:Icalendar.params * Icalendar.date_or_datetime ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  ?priority:int ->
  ?percent:int ->
  ?parent:string option ->
  t ->
  (t, [> `Msg of string ]) result

val mark_complete : t -> (t, [> `Msg of string ]) result
val set_percent : int -> t -> (t, [> `Msg of string ]) result

val todos_of_icalendar :
  string -> file:Eio.Fs.dir_ty Eio.Path.t -> Icalendar.calendar -> t list

val to_ical_todo : t -> Icalendar.todo_prop list
val to_ical_calendar : t -> Icalendar.calendar

val get_id : t -> string
val get_summary : t -> string option
val get_start : t -> Ptime.t option
val get_due : t -> Ptime.t option
val get_description : t -> string option
val get_categories : t -> string list
val get_status : t -> Icalendar.status option
val get_priority : t -> int option
val get_percent : t -> int option
val get_completed : t -> Ptime.t option
val get_calendar_name : t -> string
val get_file : t -> Eio.Fs.dir_ty Eio.Path.t
val get_related_parent : t -> string option

val is_completed : t -> bool
val is_overdue : t -> bool

type todo_tree = {
  todo : t;
  children : todo_tree list;
}

val get_ancestors : all_todos:t list -> t -> t list
val expand_with_ancestors : all_todos:t list -> filtered_todos:t list -> t list
val build_todo_tree : t list -> todo_tree list

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

val format_todo : ?format:format -> ?tz:Timedesc.Time_zone.t -> t -> string
val format_todos : ?format:format -> ?tz:Timedesc.Time_zone.t -> ?get_color:(string -> string option) -> t list -> string