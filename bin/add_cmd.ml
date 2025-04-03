open Cmdliner
open Caledonia_lib
open Event_args

let run ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~calendar_name ?timezone ?end_timezone ~fs calendar_dir
    =
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
  let calendar_name = calendar_name in
  let event =
    Event.create ~fs
      ~calendar_dir_path:(Calendar_dir.get_path calendar_dir)
      ~summary ~start ?end_ ?location ?description ?recurrence calendar_name
  in
  let* _ = Calendar_dir.add_event ~fs calendar_dir event in
  Printf.printf "Event created with ID: %s\n" (Event.get_id event);
  Ok ()

let cmd ~fs calendar_dir =
  let run summary start_date start_time end_date end_time location description
      recur calendar_name timezone end_timezone =
    match
      run ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~calendar_name ?timezone ?end_timezone ~fs
        calendar_dir
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
      $ calendar_name_arg $ timezone_arg $ end_time_arg)
  in
  let doc = "Add a new calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Add a new event to your calendar.";
      `P
        "Specify the event summary (title) as the first argument, and use \
         options to set other details.";
      `S Manpage.s_examples;
      `I
        ( "Add a event for today:",
          "caled add \"Meeting\" --date today --time 14:00" );
      `I
        ( "Add an event with a specific date and time:",
          "caled add \"Dentist Appointment\" --date 2025-04-15 --time 10:30" );
      `I
        ( "Add an event with an end time:",
          "caled add \"Conference\" --date 2025-05-20 --time 09:00 --end-date \
           2025-05-22 --end-time 17:00" );
      `I
        ( "Add an event with location and description:",
          "caled add \"Lunch with Bob\" --date 2025-04-02 --time 12:30 \
           --location \"Pasta Restaurant\" --description \"Discuss project \
           plans\"" );
      `I
        ( "Add an event to a specific calendar:",
          "caled add \"Work Meeting\" --date 2025-04-03 --time 15:00 \
           --calendar work" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "add" ~doc ~man ~exits:exit_info in
  Cmd.v info term
