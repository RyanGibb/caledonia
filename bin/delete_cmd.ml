open Cmdliner
open Caledonia_lib

let run ~component_id ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* component =
    match List.filter (fun c -> Component.get_id c = component_id) components with
    | [ comp ] -> Ok comp
    | [] -> Error (`Msg ("No component found for id " ^ component_id))
    | _ -> Error (`Msg ("More than one component found for id " ^ component_id))
  in
  let result = Calendar_dir.delete_component ~fs calendar_dir components component in
  match result with
  | Error (`Msg msg) -> Error (`Msg msg)
  | Ok _ ->
      Printf.printf "Component %s successfully deleted.\n" component_id;
      Ok ()

let component_id_arg =
  let doc = "ID of the component to delete" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

let cmd ~fs calendar_dir =
  let run component_id () =
    match run ~component_id ~fs calendar_dir with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term = Term.(const run $ component_id_arg) in
  let doc = "Delete a calendar component" in
  let man =
    [
      `S Manpage.s_description;
      `P "Delete a component (event, todo, or journal) from your calendar by its ID.";
      `P "You can find component IDs by using the `list` or `search` commands.";
      `S Manpage.s_examples;
      `P "Delete a component:";
      `P "  caled delete <id>";
      `S Manpage.s_options;
    ]
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "delete" ~doc ~man ~exits:exit_info in
  Cmd.v info term
