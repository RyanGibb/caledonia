open Cmdliner
open Caledonia_lib

let run ~event_id ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filter = Query.with_id event_id in
  let* results = Query.query_without_recurrence ~fs calendar_dir ~filter () in
  let* event =
    match results with
    | [ event ] -> Ok event
    | [] -> Error (`Msg ("No events found found for id " ^ event_id))
    | _ -> Error (`Msg ("More than one found for id " ^ event_id))
  in
  let result = Calendar_dir.delete_event ~fs calendar_dir event in
  match result with
  | Error (`Msg msg) -> Error (`Msg msg)
  | Ok () ->
      Printf.printf "Event %s successfully deleted.\n" event_id;
      Ok ()

let event_id_arg =
  let doc = "ID of the event to delete" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EVENT_ID" ~doc)

let cmd ~fs calendar_dir =
  let run event_id =
    match run ~event_id ~fs calendar_dir with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term = Term.(const run $ event_id_arg) in
  let doc = "Delete a calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Delete an event from your calendar by its ID.";
      `P "You can find event IDs by using the `list` or `search` commands.";
      `S Manpage.s_examples;
      `P "Delete an event:";
      `P "  caled delete 12345678-1234-5678-1234-567812345678";
      `S Manpage.s_options;
    ]
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "delete" ~doc ~man ~exits:exit_info in
  Cmd.v info term
