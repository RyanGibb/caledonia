open Result

let default_timezone =
  ref (fun () ->
      match Timedesc.Time_zone.local () with
      | Some tz -> tz
      | None -> Timedesc.Time_zone.utc)

let timedesc_to_ptime dt =
  match
    Timedesc.to_timestamp_single dt |> Timedesc.Utils.ptime_of_timestamp
  with
  | Some t -> t
  | None -> failwith "Invalid date conversion from Timedesc to Ptime"

let ptime_to_timedesc ?(tz = !default_timezone ()) ptime =
  let ts = Timedesc.Utils.timestamp_of_ptime ptime in
  match Timedesc.of_timestamp ~tz_of_date_time:tz ts with
  | Some dt -> dt
  | None -> failwith "Invalid date conversion from Ptime to Timedesc"

let get_today =
  ref (fun ?(tz = !default_timezone ()) () ->
      let now = Timedesc.now ~tz_of_date_time:tz () in
      let date = Timedesc.date now in
      let midnight = Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 () in
      let dt = Timedesc.of_date_and_time_exn ~tz date midnight in
      timedesc_to_ptime dt)

(* Convert a midnight timestamp to end-of-day (23:59:59) *)
let to_end_of_day date =
  let dt = ptime_to_timedesc date in
  let date = Timedesc.date dt in
  let end_of_day_time =
    Timedesc.Time.make_exn ~hour:23 ~minute:59 ~second:59 ()
  in
  let end_of_day = Timedesc.of_date_and_time_exn date end_of_day_time in
  timedesc_to_ptime end_of_day

let add_days date days =
  let dt = ptime_to_timedesc date in
  let date = Timedesc.date dt in
  let new_date = Timedesc.Date.add ~days date in
  let time = Timedesc.time dt in
  let new_dt = Timedesc.of_date_and_time_exn new_date time in
  timedesc_to_ptime new_dt

let add_weeks date weeks = add_days date (weeks * 7)

let add_months date months =
  let dt = ptime_to_timedesc date in
  let old_ym = Timedesc.ym dt in
  let year = Timedesc.Ym.year old_ym in
  let month = Timedesc.Ym.month old_ym in
  let day = Timedesc.day dt in

  (* Calculate new year and month *)
  let total_month = (year * 12) + month - 1 + months in
  let new_year = total_month / 12 in
  let new_month = (total_month mod 12) + 1 in

  (* Try to create new date, handling end of month cases properly *)
  let rec adjust_day d =
    match Timedesc.Date.Ymd.make ~year:new_year ~month:new_month ~day:d with
    | Ok new_date ->
        let time = Timedesc.time dt in
        let new_dt = Timedesc.of_date_and_time_exn new_date time in
        timedesc_to_ptime new_dt
    | Error _ ->
        if d > 1 then adjust_day (d - 1)
        else failwith "Invalid date after adding months"
  in
  adjust_day day

let add_years date years = add_months date (years * 12)

let get_start_of_week date =
  let dt = ptime_to_timedesc date in
  let day_of_week = Timedesc.weekday dt in
  let days_to_subtract =
    match day_of_week with
    | `Mon -> 0
    | `Tue -> 1
    | `Wed -> 2
    | `Thu -> 3
    | `Fri -> 4
    | `Sat -> 5
    | `Sun -> 6
  in
  let monday_date =
    Timedesc.Date.sub ~days:days_to_subtract (Timedesc.date dt)
  in
  let midnight = Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 () in
  let monday_with_midnight =
    Timedesc.of_date_and_time_exn monday_date midnight
  in
  timedesc_to_ptime monday_with_midnight

let get_start_of_current_week ?(tz = !default_timezone ()) () =
  get_start_of_week (!get_today ~tz ())

let get_start_of_next_week ?(tz = !default_timezone ()) () =
  add_days (get_start_of_current_week ~tz ()) 7

let get_end_of_week date = add_days (get_start_of_week date) 6

let get_end_of_current_week ?(tz = !default_timezone ()) () =
  get_end_of_week (!get_today ~tz ())

let get_end_of_next_week ?(tz = !default_timezone ()) () =
  get_end_of_week (get_start_of_next_week ~tz ())

let get_start_of_month date =
  let dt = ptime_to_timedesc date in
  let year = Timedesc.year dt in
  let month = Timedesc.month dt in

  (* Create a date for the first of the month *)
  match Timedesc.Date.Ymd.make ~year ~month ~day:1 with
  | Ok first_day ->
      let midnight = Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 () in
      let first_of_month = Timedesc.of_date_and_time_exn first_day midnight in
      timedesc_to_ptime first_of_month
  | Error _ -> failwith "Invalid date for start of month"

let get_start_of_current_month ?(tz = !default_timezone ()) () =
  get_start_of_month (!get_today ~tz ())

let get_start_of_next_month ?(tz = !default_timezone ()) () =
  add_months (get_start_of_current_month ~tz ()) 1

let get_end_of_month date =
  let dt = ptime_to_timedesc date in
  let year = Timedesc.year dt in
  let month = Timedesc.month dt in

  (* Determine next month and year *)
  let next_month_int = if month == 12 then 1 else month + 1 in
  let next_month_year = if month == 12 then year + 1 else year in

  (* Create a date for the first of next month *)
  match
    Timedesc.Date.Ymd.make ~year:next_month_year ~month:next_month_int ~day:1
  with
  | Ok first_of_next_month -> (
      (* Create the timestamp and subtract 1 second *)
      let midnight = Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 () in
      let first_of_next_month_dt =
        Timedesc.of_date_and_time_exn first_of_next_month midnight
      in
      let one_second = Timedesc.Span.For_human.make_exn ~seconds:1 () in
      let end_of_month_ts =
        match Timedesc.to_timestamp first_of_next_month_dt with
        | `Single ts -> Timedesc.Span.sub ts one_second
        | `Ambiguous (ts, _) -> Timedesc.Span.sub ts one_second
      in
      match Timedesc.of_timestamp end_of_month_ts with
      | Some end_of_month -> timedesc_to_ptime end_of_month
      | None -> failwith "Invalid timestamp for end of month")
  | Error _ -> failwith "Invalid date for end of month"

let get_end_of_current_month ?(tz = !default_timezone ()) () =
  get_end_of_month (!get_today ~tz ())

let get_end_of_next_month ?(tz = !default_timezone ()) () =
  get_end_of_month (get_start_of_next_month ~tz ())

let convert_relative_date_formats ?(tz = !default_timezone ()) ~today ~tomorrow
    ~week ~month () =
  if today then
    let today_date = !get_today ~tz () in
    (* Set the end date to end-of-day to include all events on that day *)
    let end_of_today = to_end_of_day today_date in
    Some (today_date, end_of_today)
  else if tomorrow then
    let today = !get_today ~tz () in
    let tomorrow_date = add_days today 1 in
    (* Set the end date to end-of-day to include all events on that day *)
    let end_of_tomorrow = to_end_of_day tomorrow_date in
    Some (tomorrow_date, end_of_tomorrow)
  else if week then
    let week_start = get_start_of_current_week ~tz () in
    let week_end_date = add_days week_start 6 in
    (* Sunday is 6 days from Monday *)
    (* Set the end date to end-of-day to include all events on Sunday *)
    let end_of_week = to_end_of_day week_end_date in
    Some (week_start, end_of_week)
  else if month then
    let month_start = get_start_of_current_month ~tz () in
    let month_end = get_end_of_month month_start in
    Some (month_start, month_end)
  else None

let ( let* ) = Result.bind

(* Parse a date string that could be ISO format or a relative expression *)
let parse_date ?(tz = !default_timezone ()) expr parameter =
  let iso_date_regex = Re.Pcre.regexp "^(\\d{4})-(\\d{2})-(\\d{2})$" in
  let relative_regex = Re.Pcre.regexp "^([+-])(\\d+)([dwm])$" in
  match expr with
  | "today" -> Ok (!get_today ~tz ())
  | "tomorrow" -> Ok (add_days (!get_today ~tz ()) 1)
  | "yesterday" -> Ok (add_days (!get_today ~tz ()) (-1))
  | "this-week" -> (
      match parameter with
      | `From -> Ok (get_start_of_current_week ~tz ())
      | `To -> Ok (get_end_of_current_week ~tz ()))
  | "next-week" -> (
      match parameter with
      | `From -> Ok (get_start_of_next_week ~tz ())
      | `To -> Ok (get_end_of_next_week ~tz ()))
  | "this-month" -> (
      match parameter with
      | `From -> Ok (get_start_of_current_month ~tz ())
      | `To -> Ok (get_end_of_current_month ~tz ()))
  | "next-month" -> (
      match parameter with
      | `From -> Ok (get_start_of_next_month ~tz ())
      | `To -> Ok (get_end_of_next_month ~tz ()))
  | _ ->
      (* Try to parse as ISO date *)
      if Re.Pcre.pmatch ~rex:iso_date_regex expr then
        let year =
          int_of_string
            (Re.Pcre.get_substring (Re.Pcre.exec ~rex:iso_date_regex expr) 1)
        in
        let month =
          int_of_string
            (Re.Pcre.get_substring (Re.Pcre.exec ~rex:iso_date_regex expr) 2)
        in
        let day =
          int_of_string
            (Re.Pcre.get_substring (Re.Pcre.exec ~rex:iso_date_regex expr) 3)
        in
        match Timedesc.Date.Ymd.make ~year ~month ~day with
        | Ok date ->
            let midnight =
              Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 ()
            in
            let dt = Timedesc.of_date_and_time_exn ~tz date midnight in
            Ok (timedesc_to_ptime dt)
        | Error _ -> Error (`Msg (Printf.sprintf "Invalid date: %s" expr))
        (* Try to parse as relative expression +Nd, -Nd, etc. *)
      else if Re.Pcre.pmatch ~rex:relative_regex expr then
        let sign =
          Re.Pcre.get_substring (Re.Pcre.exec ~rex:relative_regex expr) 1
        in
        let num =
          int_of_string
            (Re.Pcre.get_substring (Re.Pcre.exec ~rex:relative_regex expr) 2)
        in
        let unit =
          Re.Pcre.get_substring (Re.Pcre.exec ~rex:relative_regex expr) 3
        in
        let multiplier = if sign = "+" then 1 else -1 in
        let value = num * multiplier in
        let today = !get_today ~tz () in
        match unit with
        | "d" -> Ok (add_days today value)
        | "w" -> (
            let date = add_weeks today value in
            match parameter with
            | `From -> Ok (get_start_of_week date)
            | `To -> Ok (get_end_of_week date))
        | "m" -> (
            let date = add_months today value in
            match parameter with
            | `From -> Ok (get_start_of_month date)
            | `To -> Ok (get_end_of_month date))
        | _ -> Error (`Msg (Printf.sprintf "Invalid date unit: %s" unit))
      else Error (`Msg (Printf.sprintf "Invalid date format: %s" expr))

let parse_time str =
  try
    let regex =
      Re.Perl.compile_pat "^([0-9]{1,2}):([0-9]{1,2})(?::([0-9]{1,2}))?$"
    in
    match Re.exec_opt regex str with
    | Some groups ->
        let hour = int_of_string (Re.Group.get groups 1) in
        let minute = int_of_string (Re.Group.get groups 2) in
        let second =
          try int_of_string (Re.Group.get groups 3) with Not_found -> 0
        in
        if hour < 0 || hour > 23 then
          Error (`Msg (Printf.sprintf "Invalid hour: %d" hour))
        else if minute < 0 || minute > 59 then
          Error (`Msg (Printf.sprintf "Invalid minute: %d" minute))
        else if second < 0 || second > 59 then
          Error (`Msg (Printf.sprintf "Invalid second: %d" second))
        else Ok (hour, minute, second)
    | None -> Error (`Msg "Invalid time format. Expected HH:MM or HH:MM:SS")
  with e ->
    Error
      (`Msg (Printf.sprintf "Error parsing time: %s" (Printexc.to_string e)))

let parse_date_time ?(tz = !default_timezone ()) ~date ~time parameter =
  let* date_ptime = parse_date date parameter ~tz in
  let* h, min, s = parse_time time in

  let dt = ptime_to_timedesc ~tz date_ptime in
  let date_part = Timedesc.date dt in

  (* Create time *)
  match Timedesc.Time.make ~hour:h ~minute:min ~second:s () with
  | Ok time_part -> (
      (* Combine date and time *)
      match Timedesc.of_date_and_time ~tz date_part time_part with
      | Ok combined -> Ok (timedesc_to_ptime combined)
      | Error _ -> Error (`Msg "Invalid date-time combination"))
  | Error _ -> Error (`Msg "Invalid time for date-time combination")

let ptime_of_ical = function
  | `Datetime (`Utc t) -> t
  | `Datetime (`Local t) ->
      let system_tz =
        match Timedesc.Time_zone.local () with
        | Some tz -> tz
        | None -> Timedesc.Time_zone.utc
      in
      let ts = Timedesc.Utils.timestamp_of_ptime t in
      let dt =
        match Timedesc.of_timestamp ~tz_of_date_time:system_tz ts with
        | Some dt -> dt
        | None -> failwith "Invalid local date conversion"
      in
      timedesc_to_ptime dt
  | `Datetime (`With_tzid (t, (_, tzid))) ->
      let tz =
        match Timedesc.Time_zone.make tzid with
        | Some tz -> tz
        | None ->
            failwith
              (Printf.sprintf
                 "Warning: Unknown timezone %s, falling back to UTC\n" tzid)
      in
      let ts = Timedesc.Utils.timestamp_of_ptime t in
      let dt =
        match Timedesc.of_timestamp ~tz_of_date_time:tz ts with
        | Some dt -> dt
        | None -> failwith "Invalid timezone date conversion"
      in
      timedesc_to_ptime dt
  | `Date date -> (
    let y, m, d = date in
    match Timedesc.Date.Ymd.make ~year:y ~month:m ~day:d with
    | Ok new_date ->
      let midnight = Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 () in
        let new_dt = Timedesc.of_date_and_time_exn new_date midnight in
        timedesc_to_ptime new_dt
    | Error _ -> 
          failwith (Printf.sprintf "Invalid date %d-%d-%d" y m d))
