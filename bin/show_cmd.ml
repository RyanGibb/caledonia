open Cmdliner
open Caledonia_lib

let run ~event_id ~format ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filter = Event.with_id event_id in
  let* events = Calendar_dir.get_events ~fs calendar_dir in
  let results = Event.query_without_recurrence events ~filter () in
  if results = [] then print_endline "No events found."
  else print_endline (Event.format_events ~format results);
  Ok ()

let event_id_arg =
  let doc = "ID of the event to show" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EVENT_ID" ~doc)

let format_arg =
  let doc = "Output format (text, id, json, csv, ics, table, sexp)" in
  Arg.(
    value
    & opt (enum Query_args.format_enum) `Entries
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let cmd ~fs calendar_dir =
  let run event_id format () =
    match run ~event_id ~format ~fs calendar_dir with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term = Term.(const run $ event_id_arg $ format_arg) in
  let doc = "Show details of a specific event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Show detailed information about a specific event by its ID.";
      `P "You can find event IDs by using the `list` or `search` commands.";
      `S Manpage.s_examples;
      `P "Show event details:";
      `P "  caled show 12345678-1234-5678-1234-567812345678";
      `P "Show event details in JSON format:";
      `P "  caled show 12345678-1234-5678-1234-567812345678 --format json";
      `S Manpage.s_options;
    ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "show" ~doc ~man ~exits:exit_info in
  Cmd.v info term
