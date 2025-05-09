open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar ?count ?query_text ~summary ~description
    ~location ~id ~format ~today ~tomorrow ~week ~month ~recurring
    ~non_recurring ?timezone ~sort ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filters = ref [] in
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
              let* d = Date.parse_date s `From in
              Ok (Some d)
        in
        let* to_ =
          match to_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date s `To in
              Ok (Some d)
        in
        let max_date = Date.add_years (!Date.get_today ()) 75 in
        match (from, to_) with
        | Some f, Some t -> Ok (Some f, Date.to_end_of_day t)
        | Some f, None -> Ok (Some f, Date.to_end_of_day max_date)
        | None, Some t -> Ok (None, Date.to_end_of_day t)
        | None, None -> Ok (None, Date.to_end_of_day max_date))
  in
  (match calendar with
  | [] -> ()
  | calendars -> filters := Event.in_calendars calendars :: !filters);
  (match query_text with
  | Some text ->
      if summary then filters := Event.summary_contains text :: !filters;
      if description then filters := Event.description_contains text :: !filters;
      if location then filters := Event.location_contains text :: !filters;
      if not (summary || description || location) then
        filters :=
          Event.or_filter
            [
              Event.summary_contains text;
              Event.description_contains text;
              Event.location_contains text;
            ]
          :: !filters
  | None -> ());
  if recurring then filters := Event.recurring_only () :: !filters;
  if non_recurring then filters := Event.non_recurring_only () :: !filters;
  (match id with
  | Some id -> filters := Event.with_id id :: !filters
  | None -> ());
  let filter = Event.and_filter !filters in
  let comparator = Query_args.create_event_comparator sort in
  let* events = Calendar_dir.get_events ~fs calendar_dir in
  let events =
    Event.query events ~filter ~from ~to_ ~comparator ?limit:count ()
  in
  if events = [] then print_endline "No events found."
  else print_endline (Event.format_events ~tz ~format events);
  Ok ()

let query_text_arg =
  let doc =
    "Text to search for in the fields summary, description, and/or location."
  in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc)

let summary_arg =
  let doc = "Search in event summaries only" in
  Arg.(value & flag & info [ "summary"; "s" ] ~doc)

let description_arg =
  let doc = "Search in event descriptions only" in
  Arg.(value & flag & info [ "description"; "D" ] ~doc)

let location_arg =
  let doc = "Search in event locations only" in
  Arg.(value & flag & info [ "location"; "l" ] ~doc)

let recurring_arg =
  let doc = "Search for recurring events only" in
  Arg.(value & flag & info [ "recurring"; "r" ] ~doc)

let non_recurring_arg =
  let doc = "Search for non-recurring events only" in
  Arg.(value & flag & info [ "non-recurring"; "R" ] ~doc)

let id_arg =
  let doc = "Search for an event with a specific ID" in
  Arg.(value & opt (some string) None & info [ "id"; "i" ] ~docv:"ID" ~doc)

let cmd ~fs calendar_dir =
  let run query_text from_str to_str calendars count format summary description
      location id today tomorrow week month recurring non_recurring timezone
      sort () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ?query_text ~summary
        ~description ~location ~id ~format ~today ~tomorrow ~week ~month
        ~recurring ~non_recurring ?timezone ~sort ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ query_text_arg $ from_arg $ to_arg $ calendar_arg $ count_arg
      $ format_arg $ summary_arg $ description_arg $ location_arg $ id_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ recurring_arg
      $ non_recurring_arg $ timezone_arg $ sort_arg)
  in
  let doc = "Search calendar events for specific text" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Search calendar events for text in summary, description, or location \
         fields. By default, the search looks across all text fields in all \
         events regardless of date. You can specify specific fields to search \
         in using the --summary, --description, or --location flags. You can \
         use date flags to show events for a specific time period, and filter \
         events with the --sort option.\n\
        \        The search text is optional, so you search events according \
         to other query criteria.";
      `S Manpage.s_examples;
      `I ("Search for 'meeting' in all events:", "caled search meeting");
      `I
        ( "Search for 'interview' in event summaries only:",
          "caled search --summary interview" );
      `I
        ( "Search for 'conference' in a specific calendar:",
          "caled search --calendar work conference" );
      `I
        ( "Search for 'workshop' in event descriptions for today only:",
          "caled search --description --today workshop" );
      `I
        ( "Search for 'project' in events this month:",
          "caled search --month project" );
      `I
        ( "Search for 'workshop' in event descriptions within a date range:",
          "caled search --description --from 2025-03-27 --to 2025-04-01 \
           workshop" );
      `I
        ("Search for recurring events only:", "caled search --recurring meeting");
      `I
        ( "Search for non-recurring events only:",
          "caled search --non-recurring appointment" );
      `I ("Find all recurring events:", "caled search --recurring");
      `I
        ( "Find all events in a specific calendar:",
          "caled search --calendar work" );
      `I
        ( "Sort results by location and then summary:",
          "caled search --sort location --sort summary" );
      `I
        ( "Sort results by end time in descending order:",
          "caled search --sort end:desc" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "search" ~doc ~man ~exits:exit_info in
  Cmd.v info term
