open Cmdliner
open Caledonia_lib
open Query_args

let run ?from ?to_ ?calendar ?count ?query_text ~summary ~description ~location
    ~format ~today ~tomorrow ~week ~month ~recurring ~non_recurring ~fs
    calendar_dir =
  let ( let* ) = Result.bind in
  let filters = ref [] in
  let from, to_ =
    match Query.convert_relative_date_formats ~today ~tomorrow ~week ~month with
    | Some (from, to_) -> (Some from, to_)
    | None -> (
        let max_date = Query.add_years (!Query.get_today ()) 75 in
        match (from, to_) with
        | Some f, Some t -> (Some f, Query.to_end_of_day t)
        | Some f, None -> (Some f, Query.to_end_of_day max_date)
        | None, Some t -> (None, Query.to_end_of_day t)
        | None, None -> (None, Query.to_end_of_day max_date))
  in
  (match calendar with
  | Some collection_id ->
      filters :=
        Query.in_collections [ Calendar_dir.Collection collection_id ]
        :: !filters
  | None -> ());
  (match query_text with
  | Some text ->
      if summary then filters := Query.summary_contains text :: !filters;
      if description then filters := Query.description_contains text :: !filters;
      if location then filters := Query.location_contains text :: !filters;
      if not (summary || description || location) then
        filters :=
          Query.or_filter
            [
              Query.summary_contains text;
              Query.description_contains text;
              Query.location_contains text;
            ]
          :: !filters
  | None -> ());
  if recurring then filters := Query.recurring_only () :: !filters;
  if non_recurring then filters := Query.non_recurring_only () :: !filters;
  let filter = Query.and_filter !filters in
  let* results =
    Query.query ~fs calendar_dir ~filter ~from ~to_ ?limit:count ()
  in
  if results = [] then print_endline "No events found."
  else print_endline (Format.format_instances ~format results);
  Ok ()

let query_text_arg =
  let doc = "Text to search for in events (summary, description, location)" in
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

let cmd ~fs calendar_dir =
  let run query_text from to_ calendar count format summary description location
      today tomorrow week month recurring non_recurring =
    match
      run ?from ?to_ ?calendar ?count ?query_text ~summary ~description
        ~location ~format ~today ~tomorrow ~week ~month ~recurring
        ~non_recurring ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ query_text_arg $ from_arg $ to_arg $ calendar_arg $ count_arg
      $ format_arg $ summary_arg $ description_arg $ location_arg $ today_arg
      $ tomorrow_arg $ week_arg $ month_arg $ recurring_arg $ non_recurring_arg)
  in
  let doc = "Search calendar events for specific text" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Search calendar events for text in summary, description, or location \
         fields.";
      `P
        "By default, the search looks across all text fields in all events \
         regardless of date.";
      `P
        "You can narrow the search to a specific date range with date flags or \
         --from and --to.";
      `P
        "You can specify specific fields to search in using the --summary, \
         --description, or --location flags.";
      `P
        "You can limit results to only recurring or non-recurring events using \
         the --recurring or --non-recurring flags.";
      `P
        "The search text is optional if you're using other filters. For \
         example, you can find all recurring events without specifying any \
         search text.";
      `S Manpage.s_options;
      `S "DATE FORMATS";
    ]
    @ date_format_manpage_entries
    @ [
        `S "EXAMPLES";
        `P "Search for 'meeting' in all events:";
        `P "  caled search meeting";
        `P "Search for 'interview' in event summaries only:";
        `P "  caled search --summary interview";
        `P "Search for 'conference' in a specific calendar:";
        `P "  caled search --calendar work conference";
        `P "Search for 'workshop' in event descriptions for today only:";
        `P "  caled search --description --today workshop";
        `P "Search for 'project' in events this month:";
        `P "  caled search --month project";
        `P "Search for 'workshop' in event descriptions within a date range:";
        `P
          "  caled search --description --from 2025-03-27 --to 2025-04-01 \
           workshop";
        `P "Search for recurring events only:";
        `P "  caled search --recurring meeting";
        `P "Search for non-recurring events only:";
        `P "  caled search --non-recurring appointment";
        `P "Find all recurring events:";
        `P "  caled search --recurring";
        `P "Find all events in a specific calendar:";
        `P "  caled search --calendar work";
      ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in

  let info = Cmd.info "search" ~doc ~man ~exits:exit_info in
  Cmd.v info term
