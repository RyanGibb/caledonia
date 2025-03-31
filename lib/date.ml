let calendar_to_ptime date =
  let open CalendarLib in
  let year = Calendar.year date in
  let month = Date.int_of_month (Calendar.month date) in
  let day = Calendar.day_of_month date in
  let time = Calendar.to_time date in
  let hour = Time.hour time in
  let minute = Time.minute time in
  let second = Time.second time in
  match
    Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))
  with
  | Some t -> t
  | None -> failwith "Invalid date conversion from Calendar to Ptime"

let ptime_to_calendar ptime =
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time ptime
  in
  let open CalendarLib in
  let date = Date.make year month day in
  let time = Time.make hour minute second in
  Calendar.create date time

let get_today =
  ref (fun () ->
      let today_date = CalendarLib.Date.today () in
      let midnight = CalendarLib.Time.make 0 0 0 in
      let today_with_midnight =
        CalendarLib.Calendar.create today_date midnight
      in
      calendar_to_ptime today_with_midnight)

(* Convert a midnight timestamp to end-of-day (23:59:59) *)
let to_end_of_day date =
  let cal_date = ptime_to_calendar date in
  let date_only = CalendarLib.Calendar.to_date cal_date in
  let end_of_day_time = CalendarLib.Time.make 23 59 59 in
  let end_of_day = CalendarLib.Calendar.create date_only end_of_day_time in
  calendar_to_ptime end_of_day

let add_days date days =
  let cal_date = ptime_to_calendar date in
  let period = CalendarLib.Calendar.Period.day days in
  let new_date = CalendarLib.Calendar.add cal_date period in
  calendar_to_ptime new_date

let add_weeks date weeks =
  let cal_date = ptime_to_calendar date in
  let period = CalendarLib.Calendar.Period.week weeks in
  let new_date = CalendarLib.Calendar.add cal_date period in
  calendar_to_ptime new_date

let add_months date months =
  let cal_date = ptime_to_calendar date in
  let period = CalendarLib.Calendar.Period.month months in
  let new_date = CalendarLib.Calendar.add cal_date period in
  calendar_to_ptime new_date

let add_years date years =
  let cal_date = ptime_to_calendar date in
  let period = CalendarLib.Calendar.Period.year years in
  let new_date = CalendarLib.Calendar.add cal_date period in
  calendar_to_ptime new_date

let get_start_of_week date =
  let cal_date = ptime_to_calendar date in
  let day_of_week = CalendarLib.Calendar.day_of_week cal_date in
  let days_to_subtract =
    match day_of_week with
    | CalendarLib.Date.Mon -> 0
    | CalendarLib.Date.Tue -> 1
    | CalendarLib.Date.Wed -> 2
    | CalendarLib.Date.Thu -> 3
    | CalendarLib.Date.Fri -> 4
    | CalendarLib.Date.Sat -> 5
    | CalendarLib.Date.Sun -> 6
  in
  let monday =
    CalendarLib.Calendar.add cal_date
      (CalendarLib.Calendar.Period.day (-days_to_subtract))
  in
  (* Extract the date part and create a new calendar with midnight time *)
  let monday_date = CalendarLib.Calendar.to_date monday in
  let midnight = CalendarLib.Time.make 0 0 0 in
  let monday_at_midnight = CalendarLib.Calendar.create monday_date midnight in
  calendar_to_ptime monday_at_midnight

let get_start_of_current_week () = get_start_of_week (!get_today ())
let get_start_of_next_week () = add_days (get_start_of_current_week ()) 7
let get_end_of_week date = add_days (get_start_of_week date) 6
let get_end_of_current_week () = get_end_of_week (!get_today ())
let get_end_of_next_week () = get_end_of_week (get_start_of_next_week ())

let get_start_of_month date =
  let cal_date = ptime_to_calendar date in
  (* Extract year and month from calendar date *)
  let year = CalendarLib.Calendar.year cal_date in
  let month = CalendarLib.Calendar.month cal_date in
  (* Create a date for the first of the month *)
  let month_int = CalendarLib.Date.int_of_month month in
  let first_day = CalendarLib.Date.make year month_int 1 in
  let midnight = CalendarLib.Time.make 0 0 0 in
  let first_of_month = CalendarLib.Calendar.create first_day midnight in
  calendar_to_ptime first_of_month

let get_start_of_current_month () = get_start_of_month (!get_today ())
let get_start_of_next_month () = add_months (get_start_of_current_month ()) 1

let get_end_of_month date =
  let cal_date = ptime_to_calendar date in
  let year = CalendarLib.Calendar.year cal_date in
  let month = CalendarLib.Calendar.month cal_date in
  let month_int = CalendarLib.Date.int_of_month month in
  (* Create a calendar for the first of next month *)
  let next_month_int = if month_int == 12 then 1 else month_int + 1 in
  let next_month_year = if month_int == 12 then year + 1 else year in
  let first_of_next_month =
    CalendarLib.Date.make next_month_year next_month_int 1
  in
  let midnight = CalendarLib.Time.make 0 0 0 in
  let first_of_next_month_cal =
    CalendarLib.Calendar.create first_of_next_month midnight
  in
  (* Subtract one second to get the end of the current month *)
  let period = CalendarLib.Calendar.Period.second (-1) in
  let last_of_month = CalendarLib.Calendar.add first_of_next_month_cal period in
  calendar_to_ptime last_of_month

let get_end_of_current_month () = get_end_of_month (!get_today ())
let get_end_of_next_month () = get_end_of_month (get_start_of_next_month ())

let convert_relative_date_formats ~today ~tomorrow ~week ~month =
  if today then
    let today_date = !get_today () in
    (* Set the end date to end-of-day to include all events on that day *)
    let end_of_today = to_end_of_day today_date in
    Some (today_date, end_of_today)
  else if tomorrow then
    let today = !get_today () in
    let tomorrow_date = add_days today 1 in
    (* Set the end date to end-of-day to include all events on that day *)
    let end_of_tomorrow = to_end_of_day tomorrow_date in
    Some (tomorrow_date, end_of_tomorrow)
  else if week then
    let week_start = get_start_of_current_week () in
    let week_end_date = add_days week_start 6 in
    (* Sunday is 6 days from Monday *)
    (* Set the end date to end-of-day to include all events on Sunday *)
    let end_of_week = to_end_of_day week_end_date in
    Some (week_start, end_of_week)
  else if month then
    let month_start = get_start_of_current_month () in
    let month_end = get_end_of_month month_start in
    Some (month_start, month_end)
  else None

let ( let* ) = Result.bind

(* Parse a date string that could be ISO format or a relative expression *)
let parse_date expr parameter =
  let iso_date_regex = Re.Pcre.regexp "^(\\d{4})-(\\d{2})-(\\d{2})$" in
  let relative_regex = Re.Pcre.regexp "^([+-])(\\d+)([dwm])$" in
  match expr with
  | "today" -> Ok (!get_today ())
  | "tomorrow" -> Ok (add_days (!get_today ()) 1)
  | "yesterday" -> Ok (add_days (!get_today ()) (-1))
  | "this-week" -> (
      match parameter with
      | `From -> Ok (get_start_of_current_week ())
      | `To -> Ok (get_end_of_current_week ()))
  | "next-week" -> (
      match parameter with
      | `From -> Ok (get_start_of_next_week ())
      | `To -> Ok (get_end_of_next_week ()))
  | "this-month" -> (
      match parameter with
      | `From -> Ok (get_start_of_current_month ())
      | `To -> Ok (get_end_of_current_month ()))
  | "next-month" -> (
      match parameter with
      | `From -> Ok (get_start_of_next_month ())
      | `To -> Ok (get_end_of_next_month ()))
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
        match Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0)) with
        | Some date -> Ok date
        | None -> Error (`Msg (Printf.sprintf "Invalid date: %s" expr))
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
        let today = !get_today () in
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

let parse_date_time ~date ~time paramter =
  let* date = parse_date date paramter in
  let* time = parse_time time in
  let y, m, d = Ptime.to_date date in
  let h, min, s = time in
  match Ptime.of_date_time ((y, m, d), ((h, min, s), 0)) with
  | Some t -> Ok t
  | None -> Error (`Msg "Invalid date-time combination")

let parse_date_time_opt ~date ?time parameter =
  match time with
  | None -> parse_date date parameter
  | Some time -> parse_date_time ~date ~time parameter
