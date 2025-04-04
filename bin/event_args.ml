open Cmdliner
open Caledonia_lib

let calendar_name_arg =
  let doc = "Calendar to add the event to" in
  Arg.(
    required
    & opt (some string) None
    & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let required_summary_arg =
  let doc = "Event summary/title" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SUMMARY" ~doc)

let optional_summary_arg =
  let doc = "Event summary/title" in
  Arg.(
    value
    & opt (some string) None
    & info [ "summary"; "s" ] ~docv:"SUMMARY" ~doc)

let start_date_arg =
  let doc = "Event start date (YYYY-MM-DD)" in
  Arg.(value & opt (some string) None & info [ "date"; "d" ] ~docv:"DATE" ~doc)

let start_time_arg =
  let doc = "Event start time (HH:MM)" in
  Arg.(value & opt (some string) None & info [ "time"; "t" ] ~docv:"TIME" ~doc)

let end_date_arg =
  let doc = "Event end date (YYYY-MM-DD). Defaults to DATE." in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-date"; "e" ] ~docv:"END_DATE" ~doc)

let end_time_arg =
  let doc = "Event end time (HH:MM)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-time"; "T" ] ~docv:"END_TIME" ~doc)

let timezone_arg =
  let doc =
    "Timezone to add events to (e.g., 'America/New_York', 'UTC', \
     'Europe/London'). If not specified, will use the local timezone. For a \
     floating time (always at whatever the sytem time is), use 'FLOATING'."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "timezone"; "z" ] ~docv:"TIMEZONE" ~doc)

let end_timezone_arg =
  let doc = "The timezone of the end of the event. Defaults to TIMEZONE." in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-timezone"; "Z" ] ~docv:"END_TIMEZONE" ~doc)

let location_arg =
  let doc = "Event location" in
  Arg.(
    value
    & opt (some string) None
    & info [ "location"; "l" ] ~docv:"LOCATION" ~doc)

let description_arg =
  let doc = "Event description" in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "D" ] ~docv:"DESCRIPTION" ~doc)

let recur_arg =
  let doc = "See RECURRENCE section" in
  Arg.(
    value & opt (some string) None & info [ "recur"; "r" ] ~docv:"RECUR" ~doc)

let date_format_manpage_entries =
  [
    `S "DATE FORMATS";
    `P
      "The following are the possible date formats for the --date and \
       --end-date command line parameters. Note the value is dependent on \
       --date / --end-date, so --date 2025-03 --end-date 2025-03 will span the \
       month of March.";
    `I ("YYYY-MM-DD", "Specific date (e.g., 2025-3-27, zero-padding optional)");
    `I ("YYYY-MM", "Start/end of specific month (e.g., 2025-3 for March 2025)");
    `I ("YYYY", "Start/end of specific year (e.g., 2025)");
    `I ("today", "Current day");
    `I ("tomorrow", "Next day");
    `I ("yesterday", "Previous day");
    `I ("this-week", "Start/end of current week");
    `I ("next-week", "Start/end of next week");
    `I ("this-month", "Start/end of current month");
    `I ("next-month", "Start/end of next month");
    `I ("+Nd", "N days from today (e.g., +7d for a week from today)");
    `I ("-Nd", "N days before today (e.g., -7d for a week ago)");
    `I ("+Nw", "N weeks from today (e.g., +4w for 4 weeks from today)");
    `I ("+Nm", "N months from today (e.g., +2m for 2 months from today)");
  ]

let parse_start ~start_date ~start_time ~timezone =
  let ( let* ) = Result.bind in
  let* _ =
    match timezone with
    | None -> Ok ()
    | Some tzid -> (
        match Timedesc.Time_zone.make tzid with
        | Some _ -> Ok ()
        | None ->
            Error (`Msg (Printf.sprintf "Warning: Unknown timezone %s" tzid)))
  in
  match start_date with
  | None ->
      let* _ =
        match start_time with
        | None -> Ok ()
        | Some _ ->
            Error (`Msg "Can't specify an start time without an end date")
      in
      let* _ =
        match timezone with
        | None -> Ok ()
        | _ -> Error (`Msg "Can't specify a timezone without a start date")
      in
      Ok None
  | Some start_date -> (
      match start_time with
      | None ->
          let* _ =
            match timezone with
            | None -> Ok ()
            | _ -> Error (`Msg "Can't specify a timezone without a start time")
          in
          let* ptime =
            Date.parse_date ~tz:Timedesc.Time_zone.utc start_date `From
          in
          let date = Ptime.to_date ptime in
          Ok (Some (Icalendar.Params.singleton Valuetype `Date, `Date date))
      | Some start_time -> (
          match timezone with
          | None ->
              let* tzid =
                match Timedesc.Time_zone.local () with
                | Some tz -> Ok (Timedesc.Time_zone.name tz)
                | None -> Error (`Msg "Failed to get system timezone")
              in
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`With_tzid (datetime, (false, tzid)))))
          | Some "FLOATING" ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`Local datetime)))
          | Some "UTC" ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`Utc datetime)))
          | Some tzid ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`With_tzid (datetime, (false, tzid)))))))

let parse_end ~end_date ~end_time ~timezone ~end_timezone =
  let ( let* ) = Result.bind in
  let* _ =
    match timezone with
    | None -> Ok ()
    | Some tzid -> (
        match Timedesc.Time_zone.make tzid with
        | Some _ -> Ok ()
        | None ->
            Error (`Msg (Printf.sprintf "Warning: Unknown timezone %s" tzid)))
  in
  let* _ =
    match end_timezone with
    | None -> Ok ()
    | Some tzid -> (
        match Timedesc.Time_zone.make tzid with
        | Some _ -> Ok ()
        | None ->
            Error (`Msg (Printf.sprintf "Warning: Unknown timezone %s" tzid)))
  in
  match end_date with
  | None ->
      let* _ =
        match end_time with
        | None -> Ok ()
        | Some _ -> Error (`Msg "Can't specify an end time without an end date")
      in
      let* _ =
        match end_timezone with
        | None -> Ok ()
        | Some _ -> Error (`Msg "Can't specify a timezone without a end time")
      in
      Ok None
  | Some end_date -> (
      match end_time with
      | None ->
          let* _ =
            match (timezone, end_timezone) with
            | None, None -> Ok ()
            | Some _, None ->
                Error (`Msg "Can't specify a timezone without a end time")
            | _ ->
                Error (`Msg "Can't specify an end timezone without a end time")
          in
          let* ptime =
            Date.parse_date end_date ~tz:Timedesc.Time_zone.utc `From
          in
          (* DTEND;VALUE=DATE the event ends at the start of the specified date *)
          let ptime = Date.add_days ptime 1 in
          let date = Ptime.to_date ptime in
          Ok (Some (`Dtend (Icalendar.Params.singleton Valuetype `Date, `Date date)))
      | Some end_time -> (
          match (timezone, end_timezone) with
          | None, None ->
              let* tzid =
                match Timedesc.Time_zone.local () with
                | Some tz -> Ok (Timedesc.Time_zone.name tz)
                | None -> Error (`Msg "Failed to get system timezone")
              in
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend
                      ( Icalendar.Params.empty,
                        `Datetime (`With_tzid (datetime, (false, tzid))) )))
          | _, Some "FLOATING" | Some "FLOATING", None ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend (Icalendar.Params.empty, `Datetime (`Local datetime))))
          | _, Some "UTC" | Some "UTC", None ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend (Icalendar.Params.empty, `Datetime (`Utc datetime))))
          | _, Some tzid | Some tzid, _ ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend
                      ( Icalendar.Params.empty,
                        `Datetime (`With_tzid (datetime, (false, tzid))) )))))

let combine_results (results : ('a, 'b) result list) : ('a list, 'b) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | Ok v :: rest -> aux (v :: acc) rest
    | Error e :: _ -> Error e
  in
  aux [] results

let parse_recurrence recur =
  let ( let* ) = Result.bind in
  let parts = String.split_on_char ';' recur in
  let freq = ref None in
  let count = ref None in
  let until = ref None in
  let interval = ref None in
  let by_parts = ref [] in
  let results =
    List.map
      (fun part ->
        let kv = String.split_on_char '=' part in
        match kv with
        | [ "FREQ"; value ] -> (
            match String.uppercase_ascii value with
            | "DAILY" ->
                freq := Some `Daily;
                Ok ()
            | "WEEKLY" ->
                freq := Some `Weekly;
                Ok ()
            | "MONTHLY" ->
                freq := Some `Monthly;
                Ok ()
            | "YEARLY" ->
                freq := Some `Yearly;
                Ok ()
            | _ -> Error (`Msg ("Unsupported frequency: " ^ value)))
        | [ "COUNT"; value ] ->
            if !until <> None then
              Error (`Msg "Cannot use both COUNT and UNTIL in the same rule")
            else (
              count := Some (`Count (int_of_string value));
              Ok ())
        | [ "UNTIL"; value ] -> (
            if !count <> None then
              Error (`Msg "Cannot use both COUNT and UNTIL in the same rule")
            else
              let* v =
                match Icalendar.parse_datetime value with
                | Ok v -> Ok v
                | Error e -> Error (`Msg e)
              in
              match v with
              | `With_tzid _ -> Error (`Msg "Until can't be in a timezone")
              | `Utc u ->
                  until := Some (`Until (`Utc u));
                  Ok ()
              | `Local l ->
                  until := Some (`Until (`Local l));
                  Ok ())
        | [ "INTERVAL"; value ] ->
            interval := Some (int_of_string value);
            Ok ()
        | [ "BYDAY"; value ] ->
            (* Parse day specifications like MO,WE,FR or 1MO,-1FR *)
            let days = String.split_on_char ',' value in
            let parse_day day =
              (* Extract ordinal if present (like 1MO or -1FR) *)
              let ordinal, day_code =
                if
                  String.length day >= 3
                  && (String.get day 0 = '+'
                     || String.get day 0 = '-'
                     || (String.get day 0 >= '0' && String.get day 0 <= '9'))
                then (
                  let idx = ref 0 in
                  while
                    !idx < String.length day
                    && (String.get day !idx = '+'
                       || String.get day !idx = '-'
                       || String.get day !idx >= '0'
                          && String.get day !idx <= '9')
                  do
                    incr idx
                  done;
                  let ord_str = String.sub day 0 !idx in
                  let day_str =
                    String.sub day !idx (String.length day - !idx)
                  in
                  (int_of_string ord_str, day_str))
                else (0, day)
              in
              let* weekday =
                match day_code with
                | "MO" -> Ok `Monday
                | "TU" -> Ok `Tuesday
                | "WE" -> Ok `Wednesday
                | "TH" -> Ok `Thursday
                | "FR" -> Ok `Friday
                | "SA" -> Ok `Saturday
                | "SU" -> Ok `Sunday
                | _ -> Error (`Msg ("Invalid weekday: " ^ day_code))
              in
              Ok (ordinal, weekday)
            in
            let* day_specs = combine_results (List.map parse_day days) in
            by_parts := `Byday day_specs :: !by_parts;
            Ok ()
        | [ "BYMONTHDAY"; value ] ->
            let days = String.split_on_char ',' value in
            let month_days = List.map int_of_string days in
            by_parts := `Bymonthday month_days :: !by_parts;
            Ok ()
        | [ "BYMONTH"; value ] ->
            let months = String.split_on_char ',' value in
            let month_nums = List.map int_of_string months in
            by_parts := `Bymonth month_nums :: !by_parts;
            Ok ()
        | _ -> Ok ())
      parts
  in
  let* _ = combine_results results in
  match !freq with
  | Some f ->
      let limit =
        match (!count, !until) with
        | Some c, None -> Some c
        | None, Some u -> Some u
        | _ -> None
      in
      let recurrence = (f, limit, !interval, !by_parts) in
      Ok recurrence
  | None -> Error (`Msg "FREQ is required in recurrence rule")

let recurrence_format_manpage_entries =
  [
    `S "RECURRENCE";
    `P "Recurrence rule in iCalendar RFC5545 format. The FREQ part is required.";
    `I ("FREQ=<frequency>", "DAILY, WEEKLY, MONTHLY, or YEARLY (required)");
    `I
      ( "COUNT=<number>",
        "Limit to this many occurrences (optional, cannot be used with UNTIL)"
      );
    `I
      ( "UNTIL=<date>",
        "Repeat until this date (optional, cannot be used with COUNT)" );
    `I
      ( "INTERVAL=<number>",
        "Interval between occurrences, e.g., 2 for every other (optional)" );
    `I
      ( "BYDAY=<dayspec>",
        "Specific days, e.g., MO,WE,FR or 1MO (first Monday) (optional)" );
    `I
      ( "BYMONTHDAY=<daynum>",
        "Day of month, e.g., 1,15 or -1 (last day) (optional)" );
    `I
      ( "BYMONTH=<monthnum>",
        "Month number, e.g., 1,6,12 for Jan,Jun,Dec (optional)" );
    `P "Examples:";
    `I ("FREQ=DAILY;COUNT=5", "Daily for 5 occurrences");
    `I ("FREQ=WEEKLY;INTERVAL=2", "Every other week indefinitely");
    `I ("FREQ=WEEKLY;BYDAY=MO,WE,FR", "Every Monday, Wednesday, Friday");
    `I ("FREQ=MONTHLY;BYDAY=1MO", "First Monday of every month");
    `I
      ( "FREQ=YEARLY;BYMONTH=1;BYMONTHDAY=1",
        "Every January 1st (New Year's Day)" );
    `I ("FREQ=MONTHLY;BYMONTHDAY=-1", "Last day of every month");
  ]
