open Cmdliner
open Caledonia_lib
open Event_args

let run ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~collection ?timezone ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* start = parse_start ~start_date ~start_time ~timezone in
  let* start =
    match start with
    | Some s -> Ok s
    | None -> Error (`Msg "Start date required")
  in
  let* end_ = parse_end ~end_date ~end_time ~timezone ~end_timezone in
  let* recurrence =
    match recur with
    | Some r ->
        let* p = parse_recurrence r in
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

let cmd ~fs calendar_dir =
  let run summary start_date start_time end_date end_time location description
      recur collection timezone end_timezone =
    match
      run ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~collection ?timezone ?end_timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ required_summary_arg $ start_date_arg $ start_time_arg
      $ end_date_arg $ end_time_arg $ location_arg $ description_arg $ recur_arg
      $ collection_arg $ timezone_arg $ end_time_arg)
  in
  let doc = "Add a new calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Add a new event to your calendar.";
      `P
        "Specify the event summary (title) as the first argument, and use \
         options to set other details.";
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [
        `S Manpage.s_examples;
        `I
          ( "Add a event for today:",
            "caled add \"Meeting\" --date today --time 14:00" );
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
