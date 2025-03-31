open Cmdliner
open Caledonia_lib
open Query_args

let run ~event_id ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~today ~tomorrow ~week ~month ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filter = Query.with_id event_id in
  let* results = Query.query_events ~fs calendar_dir ~filter () in
  let* event =
    match results with
    | [ event ] -> Ok event
    | [] -> Error (`Msg ("No events found found for id " ^ event_id))
    | _ -> Error (`Msg ("More than one found for id " ^ event_id))
  in
  let start_date =
    match
      ( Query_args.convert_relative_date ~today ~tomorrow ~week ~month,
        start_date )
    with
    | Some date, _ -> Some date
    | None, Some date -> Some date
    | None, None -> None
  in
  let* start =
    match start_date with
    | Some date ->
        let* d = Date.parse_date_time_opt ~date ?time:start_time `From in
        Ok (Some d)
    | None -> Ok None
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
  let modifed_event =
    Event.edit ?summary ?start ?end_ ?location ?description ?recurrence event
  in
  let* _ = Calendar_dir.edit_event ~fs calendar_dir modifed_event in
  Printf.printf "Event %s updated.\n" event_id;
  Ok ()

let event_id_arg =
  let doc = "ID of the event to edit" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EVENT_ID" ~doc)

let summary_arg =
  let doc = "New event summary/title" in
  Arg.(
    value
    & opt (some string) None
    & info [ "summary"; "s" ] ~docv:"SUMMARY" ~doc)

let start_date_arg =
  let doc = "New event start date (YYYY-MM-DD)" in
  Arg.(value & opt (some string) None & info [ "date" ] ~docv:"DATE" ~doc)

let start_time_arg =
  let doc = "New event start time (HH:MM)" in
  Arg.(value & opt (some string) None & info [ "time"; "t" ] ~docv:"TIME" ~doc)

let end_date_arg =
  let doc = "New event end date (YYYY-MM-DD)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-date"; "e" ] ~docv:"END_DATE" ~doc)

let end_time_arg =
  let doc = "New event end time (HH:MM)" in
  Arg.(
    value & opt (some string) None & info [ "end-time" ] ~docv:"END_TIME" ~doc)

let location_arg =
  let doc = "New event location" in
  Arg.(
    value
    & opt (some string) None
    & info [ "location"; "l" ] ~docv:"LOCATION" ~doc)

let description_arg =
  let doc = "New event description" in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "D" ] ~docv:"DESCRIPTION" ~doc)

let recur_arg =
  let doc =
    "Recurrence rule in iCalendar RFC5545 format. The FREQ part is required.\n\
    \    Use empty string to remove recurrence.\n\
    \    \n\
    \    FREQ=<frequency>: DAILY, WEEKLY, MONTHLY, or YEARLY (required)\n\
    \    COUNT=<number>: Limit to this many occurrences (optional, cannot be \
     used with UNTIL)\n\
    \    UNTIL=<date>: Repeat until this date (optional, cannot be used with \
     COUNT)\n\
    \    INTERVAL=<number>: Interval between occurrences, e.g., 2 for every \
     other (optional)\n\
    \    BYDAY=<dayspec>: Specific days, e.g., MO,WE,FR or 1MO (first Monday) \
     (optional)\n\
    \    BYMONTHDAY=<daynum>: Day of month, e.g., 1,15 or -1 (last day) \
     (optional)\n\
    \    BYMONTH=<monthnum>: Month number, e.g., 1,6,12 for Jan,Jun,Dec \
     (optional)\n\
    \    \n\
    \    Examples:\n\
    \    FREQ=DAILY;COUNT=5         - Daily for 5 occurrences\n\
    \    FREQ=WEEKLY;INTERVAL=2     - Every other week indefinitely\n\
    \    FREQ=WEEKLY;BYDAY=MO,WE,FR - Every Monday, Wednesday, Friday\n\
    \    FREQ=MONTHLY;BYDAY=1MO     - First Monday of every month\n\
    \    FREQ=YEARLY;BYMONTH=1;BYMONTHDAY=1 - Every January 1st (New Year's Day)\n\
    \    FREQ=MONTHLY;BYMONTHDAY=-1 - Last day of every month"
  in
  Arg.(
    value & opt (some string) None & info [ "recur"; "r" ] ~docv:"RECUR" ~doc)

let cmd ~fs calendar_dir =
  let run event_id summary start_date start_time end_date end_time location
      description recur today tomorrow week month =
    match
      run ~event_id ~summary ~start_date ~start_time ~end_date ~end_time
        ~location ~description ~recur ~today ~tomorrow ~week ~month ~fs
        calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ event_id_arg $ summary_arg $ start_date_arg $ start_time_arg
      $ end_date_arg $ end_time_arg $ location_arg $ description_arg $ recur_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg)
  in
  let doc = "Edit an existing calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Edit an existing event in your calendar by its ID.";
      `P
        "Specify the event ID as the first argument, and use options to change \
         event details.";
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [
        `S Manpage.s_examples;
        `I
          ( "Change the summary of an event:",
            "caled edit 12345678-1234-5678-1234-567812345678 --summary \"New \
             Title\"" );
        `I
          ( "Change the date and time:",
            "caled edit 12345678-1234-5678-1234-567812345678 --date 2025-05-01 \
             --time 15:30" );
        `I
          ( "Update the location:",
            "caled edit 12345678-1234-5678-1234-567812345678 --location \
             \"Conference Room B\"" );
        `I
          ( "Change the description:",
            "caled edit 12345678-1234-5678-1234-567812345678 --description \
             \"Updated agenda for the meeting\"" );
      ]
  in
  let info = Cmd.info "edit" ~doc ~man in
  Cmd.v info term
