open Cmdliner
open Caledonia_lib
open Component
open Event_args
open Component_args

let parse_status s =
  match String.lowercase_ascii s with
  | "completed" -> Ok `Completed
  | "needs-action" -> Ok `Needs_action
  | "in-process" -> Ok `In_process
  | "cancelled" -> Ok `Cancelled
  | "draft" -> Ok `Draft
  | "final" -> Ok `Final
  | _ -> Error (`Msg ("Invalid status: " ^ s))

let run ~component_id ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~categories ~due_date ~due_time ~priority ~percent ~status ~parent ~no_parent
    ~alarms ~no_alarms ?timezone ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* component =
    match List.filter (fun c -> Component.get_id c = component_id) components with
    | [ comp ] -> Ok comp
    | [] -> Error (`Msg ("No component found for id " ^ component_id))
    | _ -> Error (`Msg ("More than one component found for id " ^ component_id))
  in
  match Component.component_type component with
  | CEvent ->
      let* e = match Component.to_event component with
        | Some e -> Ok e
        | None -> Error (`Msg "Failed to extract event from component")
      in
      let* start = parse_start ~start_date ~start_time ~timezone in
      let* end_ =
        let end_date =
          match (end_date, end_time) with
          | None, Some _ -> start_date
          | _ -> end_date
        in
        let end_timezone =
          match (end_date, end_time, end_timezone) with
          | Some _, Some _, None -> timezone
          | _ -> end_timezone
        in
        parse_end ~end_date ~end_time ~end_timezone
      in
      let* recurrence =
        match recur with
        | Some r ->
            let* p = parse_recurrence r in
            Ok (Some p)
        | None -> Ok None
      in
      let categories = match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* alarms_param =
        if no_alarms then Ok (Some [])
        else match alarms with
          | [] -> Ok None
          | strs -> let* a = parse_alarms strs in Ok (Some a)
      in
      let* modified = Event.edit ?summary ?start ?end_ ?location ?description ?categories ?recurrence ?alarms:alarms_param e in
      let* _ = Calendar_dir.edit_component ~fs calendar_dir components (Component.of_event modified) in
      Printf.printf "Event %s updated.\n" component_id;
      Ok ()
  | CTodo ->
      let* t = match Component.to_todo component with
        | Some t -> Ok t
        | None -> Error (`Msg "Failed to extract todo from component")
      in
      let* start = parse_start ~start_date ~start_time ~timezone in
      let* due = parse_start ~start_date:due_date ~start_time:due_time ~timezone in
      let categories = match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* status = match status with
        | Some s -> let* st = parse_status s in Ok (Some st)
        | None -> Ok None
      in
      let parent_param =
        if no_parent then Some None
        else match parent with
          | Some p -> Some (Some p)
          | None -> None
      in
      let* alarms_param =
        if no_alarms then Ok (Some [])
        else match alarms with
          | [] -> Ok None
          | strs -> let* a = parse_alarms strs in Ok (Some a)
      in
      let* modified = Todo.edit ?summary ?start ?due ?description ?categories ?status ?priority ?percent ?parent:parent_param ?alarms:alarms_param t in
      let* _ = Calendar_dir.edit_component ~fs calendar_dir components (Component.of_todo modified) in
      Printf.printf "Todo %s updated.\n" component_id;
      Ok ()
  | CJournal ->
      let* j = match Component.to_journal component with
        | Some j -> Ok j
        | None -> Error (`Msg "Failed to extract journal from component")
      in
      let* start = parse_start ~start_date ~start_time ~timezone in
      let categories = match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* status = match status with
        | Some s -> let* st = parse_status s in Ok (Some st)
        | None -> Ok None
      in
      let* modified = Journal.edit ?summary ?start ?description ?categories ?status j in
      let* _ = Calendar_dir.edit_component ~fs calendar_dir components (Component.of_journal modified) in
      Printf.printf "Journal %s updated.\n" component_id;
      Ok ()

let component_id_arg =
  let doc = "ID of the component to edit" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

let status_arg =
  let doc = "Status (for todos: completed, needs-action, in-process, cancelled; for journals: draft, final, cancelled)" in
  Arg.(value & opt (some string) None & info [ "status" ] ~docv:"STATUS" ~doc)

let cmd ~fs calendar_dir =
  let run component_id summary start_date start_time end_date end_time location
      description recur categories due_date due_time priority percent status parent no_parent
      alarms no_alarms timezone end_timezone () =
    match
      run ~component_id ~summary ~start_date ~start_time ~end_date ~end_time
        ~location ~description ~recur ~categories ~due_date ~due_time ~priority
        ~percent ~status ~parent ~no_parent ~alarms ~no_alarms ?timezone ?end_timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ component_id_arg $ optional_summary_arg $ start_date_arg
      $ start_time_arg $ end_date_arg $ end_time_arg $ location_arg
      $ description_arg $ recur_arg $ categories_arg $ due_date_arg
      $ due_time_arg $ priority_arg $ percent_arg $ status_arg $ parent_arg $ no_parent_flag
      $ alarm_arg $ no_alarms_flag $ timezone_arg $ end_timezone_arg)
  in
  let doc = "Edit an existing calendar component" in
  let man =
    [
      `S Manpage.s_description;
      `P "Edit an existing component (event, todo, or journal) in your calendar by its ID.";
      `P "Specify the component ID as the first argument, and use options to change details.";
      `S Manpage.s_examples;
      `I ("Change the summary:", "caled edit <id> --summary \"New Title\"");
      `I ("Mark a todo as completed:", "caled edit <id> --status completed");
      `I ("Set todo priority and percent:", "caled edit <id> --priority 1 --percent 50");
      `I ("Update categories:", "caled edit <id> --categories work,urgent");
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ alarm_format_manpage_entries @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "edit" ~doc ~man ~exits:exit_info in
  Cmd.v info term
