(** Core event functionality and data access *)

type event_id = string
(** Event ID type *)

type t
(** Event type representing a calendar event *)

type date_error = [ `Msg of string ]
(** Type for date-related errors *)

val create :
  summary:string ->
  start:Ptime.t ->
  ?end_:Ptime.t ->
  ?location:string ->
  ?description:string ->
  ?recurrence:Icalendar.recurrence ->
  Collection.t ->
  t
(** Create a new event with required properties *)

val edit :
  ?summary:string ->
  ?start:Ptime.t ->
  ?end_:Ptime.t ->
  ?location:string ->
  ?description:string ->
  ?recurrence:Icalendar.recurrence ->
  t ->
  t
(** Edit an existing event *)

val of_icalendar : Collection.t -> file_name:string -> Icalendar.event -> t
(** Convert an Icalendar event to our event type. *)

val to_icalendar : t -> Icalendar.event
(** Convert our event type to an Icalendar event *)

val get_id : t -> event_id
val get_summary : t -> string option
val get_start : t -> Ptime.t
val get_end : t -> Ptime.t option
val get_duration : t -> Ptime.span option
val get_day_event : t -> bool
val get_location : t -> string option
val get_description : t -> string option
val get_recurrence : t -> Icalendar.recurrence option
val get_collection : t -> Collection.t

val get_file_path :
  fs:'a Eio.Path.t -> calendar_dir_path:string -> t -> 'a Eio.Path.t
