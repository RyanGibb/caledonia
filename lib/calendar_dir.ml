open Icalendar

type t = string

let get_calendar_path ~fs calendar_dir calendar_name =
  Eio.Path.(fs / calendar_dir / calendar_name)

let ensure_dir path =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
    Ok ()
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to create directory %s: %a" (snd path) Eio.Exn.pp exn))

let create ~fs path =
  match ensure_dir Eio.Path.(fs / path) with
  | Ok () -> Ok path
  | Error e -> Error e

let get_display_name ~fs calendar_dir calendar_name =
  let displayname_path = Eio.Path.(fs / calendar_dir / calendar_name / "displayname") in
  try
    let content = Eio.Path.load displayname_path |> String.trim in
    if content = "" then calendar_name else content
  with _ -> calendar_name

let get_color ~fs calendar_dir calendar_name =
  let color_path = Eio.Path.(fs / calendar_dir / calendar_name / "color") in
  try
    let content = Eio.Path.load color_path |> String.trim in
    if content = "" then None else Some content
  with _ -> None

let list_calendar_names ~fs calendar_dir =
  try
    let dir = Eio.Path.(fs / calendar_dir) in
    let calendar_names =
      Eio.Path.read_dir dir
      |> List.filter_map (fun file ->
             if
               String.length file > 0
               && file.[0] != '.'
               && Eio.Path.is_directory Eio.Path.(dir / file)
             then Some file
             else None)
      |> List.sort (fun a b -> String.compare a b)
    in
    Ok calendar_names
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to list calendar directory %s: %a" calendar_dir
            Eio.Exn.pp exn))

let find_calendar_by_display_name ~fs calendar_dir display_name =
  match list_calendar_names ~fs calendar_dir with
  | Error _ -> None
  | Ok calendar_names ->
      (* First try to find by display name from displayname file *)
      let with_displayname_file = List.find_opt (fun cal_name ->
        let displayname_path = Eio.Path.(fs / calendar_dir / cal_name / "displayname") in
        try
          let content = Eio.Path.load displayname_path |> String.trim in
          content <> "" && String.equal content display_name
        with _ -> false
      ) calendar_names in
      match with_displayname_file with
      | Some found -> Some found
      | None ->
          (* Fall back to matching by directory name *)
          List.find_opt (fun cal_name ->
            String.equal cal_name display_name
          ) calendar_names

let rec load_components_recursive calendar_name dir_path =
  try
    Eio.Path.read_dir dir_path
    |> List.fold_left
         (fun acc name ->
           let path = Eio.Path.(dir_path / name) in
           if Eio.Path.is_directory path then
             acc @ load_components_recursive calendar_name path
           else if Filename.check_suffix name ".ics" then (
             try
               let content = Eio.Path.load path in
               match parse content with
               | Ok calendar ->
                   acc
                   @ Component.components_of_icalendar calendar_name ~file:path calendar
               | Error err ->
                   Printf.eprintf "Failed to parse %s: %s\n%!" (snd path) err;
                   acc
             with Eio.Exn.Io _ as exn ->
               Fmt.epr "Failed to read file %s: %a\n%!" (snd path) Eio.Exn.pp
                 exn;
               acc)
           else acc)
         []
  with Eio.Exn.Io _ as exn ->
    Fmt.epr "Failed to read directory %s: %a\n%!" (snd dir_path) Eio.Exn.pp exn;
    []

let get_calendar_components ~fs calendar_dir calendar_name =
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  if not (Eio.Path.is_directory calendar_name_path) then Error `Not_found
  else
    try
      let display_name = get_display_name ~fs calendar_dir calendar_name in
      let components = load_components_recursive display_name calendar_name_path in
      Ok components
    with e ->
      Error
        (`Msg
           (Printf.sprintf "Exception processing directory %s: %s"
              (snd calendar_name_path) (Printexc.to_string e)))

let get_calendar_events ~fs calendar_dir calendar_name =
  match get_calendar_components ~fs calendar_dir calendar_name with
  | Ok comps -> Ok (List.filter_map Component.to_event comps)
  | Error e -> Error e

let ( let* ) = Result.bind

let get_components ~fs calendar_dir =
  match list_calendar_names ~fs calendar_dir with
  | Error e -> Error e
  | Ok ids -> (
      try
        let rec process_ids acc = function
          | [] -> Ok (List.rev acc)
          | id :: rest -> (
              match get_calendar_components ~fs calendar_dir id with
              | Ok cal -> process_ids (cal :: acc) rest
              | Error `Not_found -> process_ids acc rest
              | Error (`Msg e) -> Error (`Msg e))
        in
        let* calendar_names = process_ids [] ids in
        Ok (List.flatten calendar_names)
      with exn ->
        Error
          (`Msg
             (Printf.sprintf "Error getting components: %s"
                (Printexc.to_string exn))))

let get_events ~fs calendar_dir =
  match get_components ~fs calendar_dir with
  | Ok comps -> Ok (List.filter_map Component.to_event comps)
  | Error e -> Error e

let add_component ~fs calendar_dir components component =
  let calendar_name = Component.get_calendar_name component in
  let file = Component.get_file component in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let calendar = Component.to_ical_calendar component in
  let content = Icalendar.to_ics ~cr:true calendar in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) file content;
    Ok (component :: components)
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let add_event ~fs calendar_dir events event =
  let components = List.map Component.of_event events in
  match add_component ~fs calendar_dir components (Component.of_event event) with
  | Ok comps -> Ok (List.filter_map Component.to_event comps)
  | Error e -> Error e

let edit_component ~fs calendar_dir components component =
  let calendar_name = Component.get_calendar_name component in
  let component_id = Component.get_id component in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let new_ical_component = Component.to_ical_component component in
  let file = Component.get_file component in
  let existing_props, existing_components = Component.to_ical_calendar component in
  let calendar =
    let has_recurrence_id (e : Icalendar.event) =
      List.exists (function `Recur_id _ -> true | _ -> false) e.props
    in
    let filtered_components =
      List.filter
        (function
          | `Event e ->
              snd e.Icalendar.uid <> component_id || has_recurrence_id e
          | `Todo (props, _) ->
              (match List.find_opt (function `Uid (_, id) -> id = component_id | _ -> false) props with
              | Some _ -> false
              | None -> true)
          | `Journal props ->
              (match List.find_opt (function `Uid (_, id) -> id = component_id | _ -> false) props with
              | Some _ -> false
              | None -> true)
          | _ -> true)
        existing_components
    in
    (existing_props, new_ical_component :: filtered_components)
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) file content;
    let filtered_components =
      List.filter (fun c -> Component.get_id c <> component_id) components
    in
    Ok (component :: filtered_components)
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let edit_event ~fs calendar_dir events event =
  let components = List.map Component.of_event events in
  match edit_component ~fs calendar_dir components (Component.of_event event) with
  | Ok comps -> Ok (List.filter_map Component.to_event comps)
  | Error e -> Error e

let delete_component ~fs calendar_dir components component =
  let calendar_name = Component.get_calendar_name component in
  let component_id = Component.get_id component in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let file = Component.get_file component in
  let existing_props, existing_components = Component.to_ical_calendar component in
  let other_components = ref false in
  let calendar =
    let filtered_components =
      List.filter
        (function
          | `Event e ->
              if snd e.Icalendar.uid = component_id then false
              else (other_components := true; true)
          | `Todo (props, _) ->
              (match List.find_opt (function `Uid (_, id) -> id = component_id | _ -> false) props with
              | Some _ -> false
              | None -> (other_components := true; true))
          | `Journal props ->
              (match List.find_opt (function `Uid (_, id) -> id = component_id | _ -> false) props with
              | Some _ -> false
              | None -> (other_components := true; true))
          | _ -> (other_components := true; true))
        existing_components
    in
    (existing_props, filtered_components)
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  try
    (match !other_components with
    | true -> Eio.Path.save ~create:(`Or_truncate 0o644) file content
    | false -> Eio.Path.unlink file);
    let filtered_components =
      List.filter (fun c -> Component.get_id c <> component_id) components
    in
    Ok filtered_components
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let delete_event ~fs calendar_dir events event =
  let components = List.map Component.of_event events in
  match delete_component ~fs calendar_dir components (Component.of_event event) with
  | Ok comps -> Ok (List.filter_map Component.to_event comps)
  | Error e -> Error e

let delete_occurrence ~fs calendar_dir events event occurrence_start =
  let modified = Event.delete_occurrence event occurrence_start in
  edit_event ~fs calendar_dir events modified

let add_occurrence_override ~fs:(_fs : Eio.Fs.dir_ty Eio.Path.t) (_calendar_dir : t) events event override_ical_event =
  let file = Event.get_file event in
  let existing_props, existing_components = Event.to_ical_calendar event in
  let calendar =
    (existing_props, existing_components @ [ `Event override_ical_event ])
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  let* () =
    try
      Eio.Path.save ~create:(`Or_truncate 0o644) file content;
      Ok ()
    with Eio.Exn.Io _ as exn ->
      Error
        (`Msg
           (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))
  in
  (* Re-read event from file to pick up the override in the calendar field *)
  let content = Eio.Path.load file in
  match Icalendar.parse content with
  | Ok new_calendar ->
      let calendar_name = Event.get_calendar_name event in
      let new_events = Event.events_of_icalendar calendar_name ~file new_calendar in
      let event_id = Event.get_id event in
      let filtered = List.filter (fun e -> Event.get_id e <> event_id) events in
      Ok (new_events @ filtered)
  | Error err ->
      Error (`Msg ("Failed to re-parse .ics after adding override: " ^ err))

let get_path t = t
