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

let pad_to_width target_width s =
  let current_width = display_width s in
  if current_width >= target_width then s
  else s ^ String.make (target_width - current_width) ' '

let max_width f data =
  List.fold_left (fun acc x -> max acc (display_width (f x))) 0 data
