open Cmdliner
open Caledonia_lib

let run ~event_id ~format ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filter = Query.with_id event_id in
  let* results = Query.query_events ~fs calendar_dir ~filter () in
  if results = [] then print_endline "No events found."
  else print_endline (Format.format_events ~format results);
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
  let run event_id format =
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
      `P
        "You can find event IDs by using the `list` or `search` commands with \
         the `id` output using `-o id`.";
      `S Manpage.s_options;
      `S Manpage.s_examples;
      `P "Show event details:";
      `P "  caled show 12345678-1234-5678-1234-567812345678";
      `P "Show event details in JSON format:";
      `P "  caled show 12345678-1234-5678-1234-567812345678 --format json";
    ]
  in
  let info = Cmd.info "show" ~doc ~man in
  Cmd.v info term
