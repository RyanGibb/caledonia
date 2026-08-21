open Cmdliner
open Caledonia_lib

let run ~component_id ~format ~timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let tz = Query_args.parse_timezone ~timezone in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let results = List.filter (fun c -> Component.get_id c = component_id) components in
  (if results = [] then print_endline "No component found."
  else
    let get_color cal_display_name =
      match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir cal_display_name with
      | Some cal_dir_name -> Calendar_dir.get_color ~fs calendar_dir cal_dir_name
      | None -> None
    in
    print_endline (Component.format_components ~format ~tz ~get_color results));
  Ok ()

let component_id_arg =
  let doc = "ID of the component to show" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

let format_arg =
  let doc = "Output format (text, id, json, csv, ics, table, sexp)" in
  Arg.(
    value
    & opt (enum Query_args.format_enum) `Entries
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let cmd ~fs calendar_dir =
  let run component_id format timezone () =
    match run ~component_id ~format ~timezone ~fs calendar_dir with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(const run $ component_id_arg $ format_arg $ Query_args.timezone_arg)
  in
  let doc = "Show details of a specific component" in
  let man =
    [
      `S Manpage.s_description;
      `P "Show detailed information about a specific component (event, todo, or journal) by its ID.";
      `P "You can find component IDs by using the `list` or `search` commands.";
      `S Manpage.s_examples;
      `P "Show component details:";
      `P "  caled show 12345678-1234-5678-1234-567812345678";
      `P "Show component details in JSON format:";
      `P "  caled show 12345678-1234-5678-1234-567812345678 --format json";
      `S Manpage.s_options;
    ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "show" ~doc ~man ~exits:exit_info in
  Cmd.v info term
