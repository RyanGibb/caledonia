type instance = { event : Event.t; start : Ptime.t; end_ : Ptime.t option }
(** Instances of recurring events with adjusted start/end times *)

module Instance : sig
  type t = instance

  type comparator = t -> t -> int
  (** Instance comparator function type *)

  val by_start : comparator
  (** Compare instances by start time, earlier times come first *)

  val by_end : comparator
  (** Compare instances by end time, earlier times come first.
      Instances with end times come after those without *)

  val by_event : Event.comparator -> comparator
  (** Apply an event comparator to instances *)

  val descending : comparator -> comparator
  (** Reverse the order of a comparator *)
end

val expand_event :
  Event.t -> from:Ptime.t option -> to_:Ptime.t -> instance list
(** Generates all instances of an event within a date range, including the
    original and recurrences. If the event result is an Error, returns an empty
    list. *)

val parse_recurrence :
  string -> (Icalendar.recurrence, [ `Msg of string ]) result
