type instance = { event : Event.t; start : Ptime.t; end_ : Ptime.t option }
(** Instances of recurring events with adjusted start/end times *)

val expand_event :
  Event.t -> from:Ptime.t option -> to_:Ptime.t -> instance list
(** Generates all instances of an event within a date range, including the
    original and recurrences. If the event result is an Error, returns an empty
    list. *)

val parse_recurrence :
  string -> (Icalendar.recurrence, [ `Msg of string ]) result
