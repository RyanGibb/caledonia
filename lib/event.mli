(** Core event functionality and data access *)

type event_id = string
(** Event ID type *)

type t
(** Event type representing a calendar event *)

type date_error = [ `Msg of string ]
(** Type for date-related errors *)

val create :
  ?collection:Calendar_dir.collection ->
  summary:string ->
  start:Ptime.t ->
  ?end_:Ptime.t ->
  ?location:string ->
  ?description:string ->
  ?recurrence:Icalendar.recurrence ->
  unit ->
  t
(** Create a new event with required properties *)

val of_icalendar :
  Calendar_dir.collection -> Icalendar.event -> (t, date_error) result
(** Convert an Icalendar event to our event type. Returns Ok with the event or
    Error with a message. *)

val to_icalendar : t -> Icalendar.event
(** Convert our event type to an Icalendar event *)

val get_id : t -> event_id
val get_summary : t -> string
val get_start : t -> Ptime.t
val get_end : t -> Ptime.t option
val get_duration : t -> Ptime.span option
val get_location : t -> string option
val get_description : t -> string option
val get_recurrence : t -> Icalendar.recurrence option
val get_collection : t -> Calendar_dir.collection option
