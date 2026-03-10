val format_date : ?tz:Timedesc.Time_zone.t -> Ptime.t -> string
val format_opt : string -> ('a -> string) -> 'a option -> string
val display_width : string -> int
val pad_to_width : ?color:string -> int -> string -> string
val max_width : ('a -> string) -> 'a list -> int
val parse_color : string -> (int * int * int) option
val colorize : ?color:string -> string -> string
val alarm_trigger : Icalendar.alarm -> (Icalendar.params * [ `Duration of Ptime.Span.t | `Datetime of Ptime.t ]) option
val format_alarm_trigger : Ptime.Span.t -> string
val format_alarm_short : Ptime.Span.t -> string
val format_alarms : Icalendar.alarm list -> string
val format_alarms_short : Icalendar.alarm list -> string
