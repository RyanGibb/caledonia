open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
    ~week ~month ?timezone ~sort ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let tz = Query_args.parse_timezone ~timezone in
  let* from, to_ =
    match
      Date.convert_relative_date_formats ~tz ~today ~tomorrow ~week ~month ()
    with
    | Some (from, to_) ->
        let* _ =
          match (from_str, to_str) with
          | None, None -> Ok ()
          | _ ->
              Error
                (`Msg
                   "Can't specify --from / --to when using --today, --week, \
                    --month")
        in
        Ok (Some from, to_)
    | None -> (
        let* from =
          match from_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date ~tz s `From in
              Ok (Some d)
        in
        let* to_ =
          match to_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date ~tz s `To in
              Ok (Some d)
        in
        match (from, to_) with
        | Some f, Some t -> Ok (Some f, Date.to_end_of_day t)
        | Some f, None ->
            let one_month_later = Date.add_months f 1 in
            Ok (Some f, one_month_later)
        | None, Some t -> Ok (None, Date.to_end_of_day t)
        | None, None ->
            let today_date = !Date.get_today ~tz () in
            let one_month_later = Date.add_months today_date 1 in
            Ok (Some today_date, one_month_later))
  in
  let filter =
    match calendars with
    | [] -> None
    | calendar -> Some (Event.in_calendars calendar)
  in
  let comparator = Query_args.create_event_comparator sort in
  let* events = Calendar_dir.get_events ~fs calendar_dir in
  let events =
    Event.query events ?filter ~from ~to_ ~comparator ?limit:count ()
  in
  if events = [] then print_endline "No events found."
  else print_endline (Event.format_events ~format ~tz events);
  Ok ()

let cmd ~fs calendar_dir =
  let run from_str to_str calendars count format today tomorrow week month
      timezone sort () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
        ~week ~month ?timezone ~sort ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ count_arg $ format_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ timezone_arg
      $ sort_arg)
  in
  let doc = "List calendar events" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List calendar events within a specified date range. By default, \
         events from today to one month from today are shown. You can use date \
         flags to show events for a specific time period, and filter events \
         with the --sort option.";
      `S Manpage.s_examples;
      `I ("List all events for today:", "caled list --today");
      `I ("List all events for tomorrow:", "caled list --tomorrow");
      `I ("List all events for the current week:", "caled list --week");
      `I ("List all events for the current month:", "caled list --month");
      `I
        ( "List events within a specific date range:",
          "caled list --from 2025-03-27 --to 2025-04-01" );
      `I ("List events from a specific calendar:", "caled list --calendar work");
      `I ("List events in JSON format:", "caled list --format json");
      `I ("Limit the number of events shown:", "caled list --count 5");
      `I
        ( "Sort by multiple fields (start time and summary):",
          "caled list --sort start --sort summary" );
      `I
        ( "Sort by calendar name in descending order:",
          "caled list --sort calendar:desc" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "list" ~doc ~man ~exits:exit_info in
  Cmd.v info term
