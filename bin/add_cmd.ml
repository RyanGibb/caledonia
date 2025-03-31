open Cmdliner
open Caledonia_lib
open Query_args

let run ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~collection ~today ~tomorrow ~week ~month ~fs
    calendar_dir =
  let ( let* ) = Result.bind in
  let* start_date =
    match
      ( Query_args.convert_relative_date ~today ~tomorrow ~week ~month,
        start_date )
    with
    | Some date, _ -> Ok date
    | None, Some date -> Ok date
    | None, None -> Error (`Msg "Start date is required")
  in
  let* start =
    Date.parse_date_time_opt ~date:start_date ?time:start_time `From
  in
  let end_date =
    match
      (Query_args.convert_relative_date ~today ~tomorrow ~week ~month, end_date)
    with
    | Some date, _ -> Some date
    | None, Some date -> Some date
    | None, None -> None
  in
  let* end_ =
    match end_date with
    | None -> Ok None
    | Some end_date ->
        let* end_dt =
          Date.parse_date_time_opt ~date:end_date ?time:end_time `To
        in
        Ok (Some end_dt)
  in
  let* recurrence =
    match recur with
    | Some r ->
        let* p = Recur.parse_recurrence r in
        Ok (Some p)
    | None -> Ok None
  in
  let collection = Collection.Col collection in
  let event =
    Event.create ~summary ~start ?end_ ?location ?description ?recurrence
      collection
  in
  let* _ = Calendar_dir.add_event ~fs calendar_dir event in
  Printf.printf "Event created with ID: %s\n" (Event.get_id event);
  Ok ()

let collection_arg =
  let doc = "Calendar to add the event to" in
  Arg.(
    required
    & opt (some string) None
    & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let summary_arg =
  let doc = "Event summary/title" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SUMMARY" ~doc)

let start_date_arg =
  let doc = "Event start date (YYYY-MM-DD)" in
  Arg.(value & opt (some string) None & info [ "date" ] ~docv:"DATE" ~doc)

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
    value & opt (some string) None & info [ "end-time" ] ~docv:"END_TIME" ~doc)

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

let cmd ~fs calendar_dir =
  let run summary start_date start_time end_date end_time location description
      recur collection today tomorrow week month =
    match
      run ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~collection ~today ~tomorrow ~week ~month ~fs
        calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ summary_arg $ start_date_arg $ start_time_arg $ end_date_arg
      $ end_time_arg $ location_arg $ description_arg $ recur_arg
      $ collection_arg $ today_arg $ tomorrow_arg $ week_arg $ month_arg)
  in
  let doc = "Add a new calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Add a new event to your calendar.";
      `P
        "Specify the event summary (title) as the first argument, and use \
         options to set other details.";
      `P "Note all times are in Coordinated Universal Time (UTC).";
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [
        `S Manpage.s_examples;
        `I
          ( "Add a simple event for today:",
            "caled add \"Team Meeting\" --today --time 14:00" );
        `I
          ( "Add an event with a specific date and time:",
            "caled add \"Dentist Appointment\" --date 2025-04-15 --time 10:30"
          );
        `I
          ( "Add an event with an end time:",
            "caled add \"Conference\" --date 2025-05-20 --time 09:00 \
             --end-date 2025-05-22 --end-time 17:00" );
        `I
          ( "Add an event with location and description:",
            "caled add \"Lunch with Bob\" --date 2025-04-02 --time 12:30 \
             --location \"Pasta Restaurant\" --description \"Discuss project \
             plans\"" );
        `I
          ( "Add an event to a specific calendar:",
            "caled add \"Work Meeting\" --date 2025-04-03 --time 15:00 \
             --calendar work" );
        `S "RECURRENCE";
        `P
          "Recurrence rule in iCalendar RFC5545 format. The FREQ part is \
           required.";
        `I ("FREQ=<frequency>", "DAILY, WEEKLY, MONTHLY, or YEARLY (required)");
        `I
          ( "COUNT=<number>",
            "Limit to this many occurrences (optional, cannot be used with \
             UNTIL)" );
        `I
          ( "UNTIL=<date>",
            "Repeat until this date (optional, cannot be used with COUNT)" );
        `I
          ( "INTERVAL=<number>",
            "Interval between occurrences, e.g., 2 for every other (optional)"
          );
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
  in
  let info = Cmd.info "add" ~doc ~man in
  Cmd.v info term
