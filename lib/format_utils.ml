let format_date ?tz date =
  let dt = Date.ptime_to_timedesc ?tz date in
  let y = Timedesc.year dt in
  let m = Timedesc.month dt in
  let d = Timedesc.day dt in
  let weekday =
    match Timedesc.weekday dt with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  Printf.sprintf "%04d-%02d-%02d %s" y m d weekday

let format_opt label f opt =
  Option.map (fun x -> Printf.sprintf "%s: %s\n" label (f x)) opt
  |> Option.value ~default:""

let display_width s =
  Uutf.String.fold_utf_8 (fun acc _ decoded ->
    match decoded with
    | `Uchar uchar ->
        let width = Uucp.Break.tty_width_hint uchar in
        acc + width
    | `Malformed _ -> acc + 1
  ) 0 s

let parse_color color_str =
  let color_str = String.trim color_str in
  if String.length color_str > 0 && color_str.[0] = '#' then
    let hex = String.sub color_str 1 (String.length color_str - 1) in
    try
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Some (r, g, b)
    with _ -> None
  else None

let colorize ?color text =
  match color with
  | None -> text
  | Some color_str ->
      (match parse_color color_str with
      | None -> text
      | Some (r, g, b) ->
          Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text)

let pad_to_width ?(color:string option) target_width s =
  let current_width = display_width s in
  let padded =
    if current_width >= target_width then s
    else s ^ String.make (target_width - current_width) ' '
  in
  match color with
  | None -> padded
  | Some c -> colorize ~color:c padded

let max_width f data =
  List.fold_left (fun acc x -> max acc (display_width (f x))) 0 data

let format_alarm_trigger span =
  let seconds = Ptime.Span.to_float_s span in
  let abs_seconds = Float.abs seconds in
  let suffix = if seconds < 0.0 then " before" else if seconds > 0.0 then " after" else "" in
  let days = int_of_float (abs_seconds /. 86400.0) in
  let remaining = abs_seconds -. (float_of_int days *. 86400.0) in
  let hours = int_of_float (remaining /. 3600.0) in
  let remaining = remaining -. (float_of_int hours *. 3600.0) in
  let minutes = int_of_float (remaining /. 60.0) in
  let parts = [] in
  let parts = if days > 0 then
    (Printf.sprintf "%d day%s" days (if days > 1 then "s" else "")) :: parts
  else parts in
  let parts = if hours > 0 then
    (Printf.sprintf "%d hour%s" hours (if hours > 1 then "s" else "")) :: parts
  else parts in
  let parts = if minutes > 0 then
    (Printf.sprintf "%d minute%s" minutes (if minutes > 1 then "s" else "")) :: parts
  else parts in
  let parts = List.rev parts in
  match parts with
  | [] -> "at start"
  | _ -> String.concat " " parts ^ suffix

let format_alarm_short span =
  let seconds = Ptime.Span.to_float_s span in
  let abs_seconds = Float.abs seconds in
  let days = int_of_float (abs_seconds /. 86400.0) in
  let remaining = abs_seconds -. (float_of_int days *. 86400.0) in
  let hours = int_of_float (remaining /. 3600.0) in
  let remaining = remaining -. (float_of_int hours *. 3600.0) in
  let minutes = int_of_float (remaining /. 60.0) in
  if days > 0 && hours = 0 && minutes = 0 then Printf.sprintf "%dd" days
  else if days = 0 && hours > 0 && minutes = 0 then Printf.sprintf "%dh" hours
  else if days = 0 && hours = 0 && minutes > 0 then Printf.sprintf "%dm" minutes
  else if days = 0 && hours = 0 && minutes = 0 then "0m"
  else
    let parts = [] in
    let parts = if minutes > 0 then Printf.sprintf "%dm" minutes :: parts else parts in
    let parts = if hours > 0 then Printf.sprintf "%dh" hours :: parts else parts in
    let parts = if days > 0 then Printf.sprintf "%dd" days :: parts else parts in
    String.concat "" parts

let alarm_trigger = function
  | `Audio a -> Some a.Icalendar.trigger
  | `Display a -> Some a.Icalendar.trigger
  | `Email a -> Some a.Icalendar.trigger
  | `None _ -> None

let format_alarms alarms =
  let trigger_strs = List.filter_map (fun alarm ->
    match alarm_trigger alarm with
    | Some (_, `Duration span) -> Some (format_alarm_trigger span)
    | Some (_, `Datetime _) -> Some "at fixed time"
    | None -> None
  ) alarms in
  match trigger_strs with
  | [] -> ""
  | strs -> String.concat ", " strs

let format_alarms_short alarms =
  let trigger_strs = List.filter_map (fun alarm ->
    match alarm_trigger alarm with
    | Some (_, `Duration span) -> Some (format_alarm_short span)
    | Some (_, `Datetime _) -> Some "abs"
    | None -> None
  ) alarms in
  match trigger_strs with
  | [] -> ""
  | strs -> String.concat "," strs
