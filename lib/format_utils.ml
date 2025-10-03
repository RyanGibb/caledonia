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
