open Cmdliner
open Caledonia_lib

let collection_arg =
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
  let doc = "Event end date (YYYY-MM-DD)" in
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
     'Europe/London'). If not specified, will use the local timezone."
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
    `P "Relative date formats for --date / -d and --end-date / -e:";
    `I ("today", "Current day");
    `I ("tomorrow", "Next day");
    `I ("yesterday", "Previous day");
    `I ("this-week", "Start of current week");
    `I ("next-week", "Start of next week");
    `I ("this-month", "Start of current month");
    `I ("next-month", "Start of next month");
    `I ("+Nd", "N days from today (e.g., +7d for a week from today)");
    `I ("-Nd", "N days before today (e.g., -7d for a week ago)");
    `I ("+Nw", "N weeks from today (e.g., +4w for 4 weeks from today)");
    `I ("+Nm", "N months from today (e.g., +2m for 2 months from today)");
  ]

let parse_start ~start_date ~start_time ~timezone =
  let ( let* ) = Result.bind in
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
          Ok (Some (`Date date))
      | Some start_time -> (
          match timezone with
          | None ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (`Datetime (`Local datetime)))
          | Some "UTC" ->
              let* datetime =
                Date.parse_date_time ~tz:Timedesc.Time_zone.utc ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (`Datetime (`Utc datetime)))
          | Some tzid ->
              let* tz =
                match Timedesc.Time_zone.make tzid with
                | Some tz_obj -> Ok tz_obj
                | None -> Error (`Msg ("Invalid timezone: " ^ tzid))
              in
              let* datetime =
                Date.parse_date_time ~tz ~date:start_date ~time:start_time `From
              in
              Ok (Some (`Datetime (`With_tzid (datetime, (false, tzid)))))))

let parse_end ~end_date ~end_time ~timezone ~end_timezone =
  let ( let* ) = Result.bind in
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
          let date = Ptime.to_date ptime in
          Ok (Some (`Dtend (Icalendar.Params.empty, `Date date)))
      | Some end_time -> (
          match (timezone, end_timezone) with
          | None, None ->
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
              let* tz =
                match Timedesc.Time_zone.make tzid with
                | Some tz_obj -> Ok tz_obj
                | None -> Error (`Msg ("Invalid timezone: " ^ tzid))
              in
              let* datetime =
                Date.parse_date_time ~tz ~date:end_date ~time:end_time `From
              in

              Ok
                (Some
                   (`Dtend
                      ( Icalendar.Params.empty,
                        `Datetime (`With_tzid (datetime, (false, tzid))) )))))
