val format_date : ?tz:Timedesc.Time_zone.t -> Ptime.t -> string
val format_opt : string -> ('a -> string) -> 'a option -> string
val display_width : string -> int
val pad_to_width : ?color:string -> int -> string -> string
val max_width : ('a -> string) -> 'a list -> int
val parse_color : string -> (int * int * int) option
val colorize : ?color:string -> string -> string
