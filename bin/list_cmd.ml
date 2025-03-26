open Cmdliner
open Caledonia_lib
open Query_args

let run ?from ?to_ ?calendar ?count ~format ~today ~tomorrow ~week ~month ~fs
    calendar_dir =
  let ( let* ) = Result.bind in
  let from, to_ =
    match Query.convert_relative_date_formats ~today ~tomorrow ~week ~month with
    | Some (from, to_) -> (Some from, to_)
    | None -> (
        match (from, to_) with
        | Some f, Some t -> (Some f, Query.to_end_of_day t)
        | Some f, None ->
            let one_month_later = Query.add_months f 1 in
            (Some f, one_month_later)
        | None, Some t ->
            let today_date = !Query.get_today () in
            (Some today_date, Query.to_end_of_day t)
        | None, None ->
            let today_date = !Query.get_today () in
            let one_month_later = Query.add_months today_date 1 in
            (Some today_date, one_month_later))
  in
  let filter =
    match calendar with
    | Some collection_id ->
        Some (Query.in_collections [ Calendar_dir.Collection collection_id ])
    | None -> None
  in
  let* results =
    Query.query ~fs calendar_dir ?filter ~from ~to_ ?limit:count ()
  in
  if results = [] then print_endline "No events found."
  else print_endline (Format.format_instances ~format results);
  Ok ()

let cmd ~fs calendar_dir =
  let run from to_ calendar count format today tomorrow week month =
    match
      run ?from ?to_ ?calendar ?count ~format ~today ~tomorrow ~week ~month ~fs
        calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ count_arg $ format_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg)
  in
  let doc = "List calendar events" in
  let man =
    [
      `S Manpage.s_description;
      `P "List calendar events within a specified date range.";
      `P "By default, events from today to one month from today are shown.";
      `P "You can use date flags to show events for a specific time period.";
      `P "You can also filter events by calendar using the --calendar flag.";
      `S Manpage.s_options;
      `S "DATE FORMATS";
    ]
    @ date_format_manpage_entries
    @ [
        `S "EXAMPLES";
        `P "List all events for today:";
        `P "  caled list --today";
        `P "List all events for tomorrow:";
        `P "  caled list --tomorrow";
        `P "List all events for the current week:";
        `P "  caled list --week";
        `P "List all events for the current month:";
        `P "  caled list --month";
        `P "List events within a specific date range:";
        `P "  caled list --from 2025-03-27 --to 2025-04-01";
        `P "List events from a specific calendar:";
        `P "  caled list --calendar work";
        `P "List events in JSON format:";
        `P "  caled list --format json";
        `P "Limit the number of events shown:";
        `P "  caled list --count 5";
      ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in

  let info = Cmd.info "list" ~doc ~man ~exits:exit_info in
  Cmd.v info term
