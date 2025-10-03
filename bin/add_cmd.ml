open Cmdliner
open Caledonia_lib
open Event_args
open Component_args

let parse_categories cats =
  match cats with
  | None -> []
  | Some s -> String.split_on_char ',' s |> List.map String.trim

let run_event ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~categories ~calendar_name ?timezone ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* start = parse_start ~start_date ~start_time ~timezone in
  let* start =
    match start with
    | Some s -> Ok s
    | None -> Error (`Msg "Start date required for events")
  in
  let* end_ =
    let end_date =
      match (end_date, end_time) with
      | None, Some _ -> start_date
      | _ -> end_date
    in
    let end_date =
      match (start_date, end_date) with
      | Some _, None -> start_date
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
  let categories = parse_categories categories in
  let categories_opt = match categories with [] -> None | cats -> Some cats in
  let* event =
    Event.create ~fs
      ~calendar_dir_path:(Calendar_dir.get_path calendar_dir)
      ~summary ~start ?end_ ?location ?description ?categories:categories_opt ?recurrence calendar_name
  in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* _ = Calendar_dir.add_component ~fs calendar_dir components (Component.of_event event) in
  Printf.printf "Event created with ID: %s\n" (Event.get_id event);
  Ok ()

let run_todo ~summary ~start_date ~start_time ~due_date ~due_time ~description
    ~categories ~priority ~percent ~status ~parent ~calendar_name ?timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* start = parse_start ~start_date ~start_time ~timezone in
  let* due = parse_start ~start_date:due_date ~start_time:due_time ~timezone in
  let categories = parse_categories categories in
  let* todo =
    Todo.create ~fs
      ~calendar_dir_path:(Calendar_dir.get_path calendar_dir)
      ?summary ?start ?due ?description
      ~categories ?status ?priority ?percent ?parent calendar_name
  in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* _ = Calendar_dir.add_component ~fs calendar_dir components (Component.of_todo todo) in
  Printf.printf "Todo created with ID: %s\n" (Todo.get_id todo);
  Ok ()

let run_journal ~summary ~start_date ~start_time ~description ~categories ~status
    ~calendar_name ?timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let* start = parse_start ~start_date ~start_time ~timezone in
  let categories = parse_categories categories in
  let* journal =
    Journal.create ~fs
      ~calendar_dir_path:(Calendar_dir.get_path calendar_dir)
      ?summary ?start ?description ~categories ?status calendar_name
  in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* _ = Calendar_dir.add_component ~fs calendar_dir components (Component.of_journal journal) in
  Printf.printf "Journal created with ID: %s\n" (Journal.get_id journal);
  Ok ()

let run ~component_type ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~categories ~due_date ~due_time ~priority ~percent ~status ~parent
    ~calendar_name ?timezone ?end_timezone ~fs calendar_dir =
  let calendar_name =
    match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir calendar_name with
    | Some name -> name
    | None -> calendar_name
  in
  match component_type with
  | "event" ->
      run_event ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~categories ~calendar_name ?timezone ?end_timezone ~fs calendar_dir
  | "todo" ->
      run_todo ~summary:(Some summary) ~start_date ~start_time ~due_date ~due_time ~description
        ~categories ~priority ~percent ~status ~parent ~calendar_name ?timezone ~fs calendar_dir
  | "journal" ->
      run_journal ~summary:(Some summary) ~start_date ~start_time ~description ~categories ~status
        ~calendar_name ?timezone ~fs calendar_dir
  | _ -> Error (`Msg "Invalid component type")

let cmd ~fs calendar_dir =
  let run component_type summary start_date start_time end_date end_time location description
      recur categories due_date due_time priority percent status parent calendar_name timezone end_timezone () =
    match
      run ~component_type ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~categories ~due_date ~due_time ~priority ~percent ~status ~parent
        ~calendar_name ?timezone ?end_timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ component_type_arg $ required_summary_arg $ start_date_arg $ start_time_arg
      $ end_date_arg $ end_time_arg $ location_arg $ description_arg $ recur_arg
      $ categories_arg $ due_date_arg $ due_time_arg $ priority_arg $ percent_arg $ status_arg
      $ parent_arg $ calendar_name_arg $ timezone_arg $ end_timezone_arg)
  in
  let doc = "Add a new calendar component (event, todo, or journal)" in
  let man =
    [
      `S Manpage.s_description;
      `P "Add a new event, todo, or journal entry to your calendar.";
      `P
        "Specify the component summary (title) as the first argument, and use \
         options to set other details. Use --type to specify the component type.";
      `S Manpage.s_examples;
      `I
        ( "Add an event for today:",
          "caled add \"Meeting\" --date today --time 14:00" );
      `I
        ( "Add a todo with a due date:",
          "caled add --type todo \"Fix bug\" --due 2025-04-15 --priority 1" );
      `I
        ( "Add a journal entry:",
          "caled add --type journal \"Daily notes\" --description \"...\" --categories work,ideas" );
      `I
        ( "Add an event with location and description:",
          "caled add \"Lunch with Bob\" --date 2025-04-02 --time 12:30 \
           --location \"Pasta Restaurant\" --description \"Discuss project plans\"" );
      `I
        ( "Add a todo with percent complete:",
          "caled add --type todo \"Write report\" --due 2025-05-01 --percent 50" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "add" ~doc ~man ~exits:exit_info in
  Cmd.v info term