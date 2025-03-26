type filter =
  | SummaryContains of string
  | DescriptionContains of string
  | LocationContains of string
  | InCollections of Calendar_dir.collection list
  | RecurringOnly
  | NonRecurringOnly
  | WithId of Event.event_id
  | And of filter list
  | Or of filter list
  | Not of filter

type sort_order = [ `Ascending | `Descending ]
type sort_by = [ `Start | `End | `Summary | `Location | `Calendar ]

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

(* Parse a date string that could be ISO format or a relative expression *)
let parse_date_expression expr parameter =
  let iso_date_regex = Re.Pcre.regexp "^(\\d{4})-(\\d{2})-(\\d{2})$" in
  let relative_regex = Re.Pcre.regexp "^([+-])(\\d+)([dwm])$" in
  match expr with
  | "today" -> !get_today ()
  | "tomorrow" -> add_days (!get_today ()) 1
  | "yesterday" -> add_days (!get_today ()) (-1)
  | "this-week" -> (
      match parameter with
      | `From -> get_start_of_current_week ()
      | `To -> get_end_of_current_week ())
  | "next-week" -> (
      match parameter with
      | `From -> get_start_of_next_week ()
      | `To -> get_end_of_next_week ())
  | "this-month" -> (
      match parameter with
      | `From -> get_start_of_current_month ()
      | `To -> get_end_of_current_month ())
  | "next-month" -> (
      match parameter with
      | `From -> get_start_of_next_month ()
      | `To -> get_end_of_next_month ())
  | _ ->
      (* Try to parse as ISO date *)
      if Re.Pcre.pmatch ~rex:iso_date_regex expr then
        try
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
          | Some date -> date
          | None -> failwith (Printf.sprintf "Invalid date: %s" expr)
        with e ->
          failwith
            (Printf.sprintf "Failed to parse ISO date '%s': %s" expr
               (Printexc.to_string e))
        (* Try to parse as relative expression +Nd, -Nd, etc. *)
      else if Re.Pcre.pmatch ~rex:relative_regex expr then
        try
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
          | "d" -> add_days today value
          | "w" -> (
              let date = add_weeks today value in
              match parameter with
              | `From -> get_start_of_week date
              | `To -> get_end_of_week date)
          | "m" -> (
              let date = add_months today value in
              match parameter with
              | `From -> get_start_of_month date
              | `To -> get_end_of_month date)
          | _ -> failwith (Printf.sprintf "Invalid date unit: %s" unit)
        with e ->
          failwith
            (Printf.sprintf "Failed to parse relative date '%s': %s" expr
               (Printexc.to_string e))
      else failwith (Printf.sprintf "Invalid date format: %s" expr)

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

let summary_contains text = SummaryContains text
let description_contains text = DescriptionContains text
let location_contains text = LocationContains text
let in_collections ids = InCollections ids
let recurring_only () = RecurringOnly
let non_recurring_only () = NonRecurringOnly
let with_id id = WithId id
let and_filter filters = And filters
let or_filter filters = Or filters
let not_filter filter = Not filter

let rec matches_filter event = function
  | SummaryContains text ->
      let summary = Event.get_summary event in
      let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
      Re.Pcre.pmatch ~rex:re summary
  | DescriptionContains text -> (
      match Event.get_description event with
      | Some desc ->
          let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
          Re.Pcre.pmatch ~rex:re desc
      | None -> false)
  | LocationContains text -> (
      match Event.get_location event with
      | Some loc ->
          let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
          Re.Pcre.pmatch ~rex:re loc
      | None -> false)
  | InCollections ids -> (
      match Event.get_collection event with
      | Some id -> List.exists (fun col -> col = id) ids
      | None -> false)
  | RecurringOnly -> Event.get_recurrence event <> None
  | NonRecurringOnly -> Event.get_recurrence event = None
  | WithId id -> Event.get_id event = id
  | And filters -> List.for_all (matches_filter event) filters
  | Or filters -> List.exists (matches_filter event) filters
  | Not filter -> not (matches_filter event filter)

let compare_events sort_by order e1 e2 =
  let compare =
    match sort_by with
    | `Start ->
        let t1 = Event.get_start e1 in
        let t2 = Event.get_start e2 in
        Ptime.compare t1 t2
    | `End -> (
        match (Event.get_end e1, Event.get_end e2) with
        | Some t1, Some t2 -> Ptime.compare t1 t2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
    | `Summary -> String.compare (Event.get_summary e1) (Event.get_summary e2)
    | `Location -> (
        match (Event.get_location e1, Event.get_location e2) with
        | Some l1, Some l2 -> String.compare l1 l2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
    | `Calendar -> (
        match (Event.get_collection e1, Event.get_collection e2) with
        | Some (Calendar_dir.Collection c1), Some (Calendar_dir.Collection c2)
          ->
            String.compare c1 c2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
  in
  match order with `Ascending -> compare | `Descending -> -compare

let get_all_events ~fs calendar_dir =
  match Calendar_dir.get_collections ~fs calendar_dir with
  | Ok collections ->
      let events =
        List.concat_map
          (fun (collection, calendar_files) ->
            List.concat_map
              (fun calendar_file ->
                List.filter_map
                  (function
                    | `Event e -> (
                        match Event.of_icalendar collection e with
                        | Ok event -> Some event
                        | Error (`Msg msg) ->
                            Printf.eprintf "Error parsing event from %s: %s\n%!"
                              calendar_file.Calendar_dir.file_path msg;
                            None)
                    | _ -> None)
                  (snd calendar_file.Calendar_dir.calendar))
              calendar_files)
          collections
      in
      Ok events
  | Error e -> Error e

let query_events ~fs calendar_dir ?filter ?sort_by ?order ?limit () =
  match get_all_events ~fs calendar_dir with
  | Ok events ->
      let filtered_events =
        match filter with
        | Some f -> List.filter (fun event -> matches_filter event f) events
        | None -> events
      in
      let sorted_events =
        match (sort_by, order) with
        | Some criteria, Some ord ->
            List.sort (compare_events criteria ord) filtered_events
        | Some criteria, None ->
            List.sort (compare_events criteria `Ascending) filtered_events
        | None, _ ->
            List.sort (compare_events `Start `Ascending) filtered_events
      in
      Ok
        (match limit with
        | Some n when n > 0 ->
            let rec take n lst acc =
              match (lst, n) with
              | _, 0 -> List.rev acc
              | [], _ -> List.rev acc
              | x :: xs, n -> take (n - 1) xs (x :: acc)
            in
            take n sorted_events []
        | _ -> sorted_events)
  | Error e -> Error e

let query ~fs calendar_dir ?filter ~from ~to_ ?sort_by ?order ?limit () =
  match query_events ~fs calendar_dir ?filter ?sort_by ?order () with
  | Ok events ->
      let instances =
        List.concat_map
          (fun event -> Recur.expand_event event ~from ~to_)
          events
      in
      let compare_instances criteria ord i1 i2 =
        match criteria with
        | `Start ->
            let c = Ptime.compare i1.Recur.start i2.Recur.start in
            if ord = `Ascending then c else -c
        | `End -> (
            match (i1.Recur.end_, i2.Recur.end_) with
            | Some t1, Some t2 ->
                let c = Ptime.compare t1 t2 in
                if ord = `Ascending then c else -c
            | Some _, None -> if ord = `Ascending then 1 else -1
            | None, Some _ -> if ord = `Ascending then -1 else 1
            | None, None -> 0)
        | other ->
            let c = compare_events other ord i1.Recur.event i2.Recur.event in
            if ord = `Ascending then c else -c
      in
      let sorted_instances =
        match (sort_by, order) with
        | Some criteria, Some ord ->
            List.sort (compare_instances criteria ord) instances
        | Some criteria, None ->
            List.sort (compare_instances criteria `Ascending) instances
        | None, _ -> List.sort (compare_instances `Start `Ascending) instances
      in
      Ok
        (match limit with
        | Some n when n > 0 ->
            let rec take n lst acc =
              match (lst, n) with
              | _, 0 -> List.rev acc
              | [], _ -> List.rev acc
              | x :: xs, n -> take (n - 1) xs (x :: acc)
            in
            take n sorted_instances []
        | _ -> sorted_instances)
  | Error e -> Error e
