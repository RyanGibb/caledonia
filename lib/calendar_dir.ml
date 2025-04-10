open Icalendar

type t = string

let get_calendar_path ~fs calendar_dir calendar_name_name =
  Eio.Path.(fs / calendar_dir / calendar_name_name)

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

let rec load_events_recursive calendar_name dir_path =
  try
    Eio.Path.read_dir dir_path
    |> List.fold_left
         (fun acc name ->
           let path = Eio.Path.(dir_path / name) in
           if Eio.Path.is_directory path then
             acc @ load_events_recursive calendar_name path
           else if Filename.check_suffix name ".ics" then (
             try
               let content = Eio.Path.load path in
               match parse content with
               | Ok calendar ->
                   acc
                   @ Event.events_of_icalendar ~file:path calendar_name calendar
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

let get_calendar_events ~fs calendar_dir calendar_name =
  let calendar_name_path =
    get_calendar_path ~fs calendar_dir calendar_name
  in
  if not (Eio.Path.is_directory calendar_name_path) then Error `Not_found
  else
    try
      let events = load_events_recursive calendar_name calendar_name_path in
      Ok events
    with e ->
      Error
        (`Msg
           (Printf.sprintf "Exception processing directory %s: %s"
              (snd calendar_name_path) (Printexc.to_string e)))

let ( let* ) = Result.bind

let get_events ~fs calendar_dir =
  match list_calendar_names ~fs calendar_dir with
  | Error e -> Error e
  | Ok ids -> (
      try
        let rec process_ids acc = function
          | [] -> Ok (List.rev acc)
          | id :: rest -> (
              match get_calendar_events ~fs calendar_dir id with
              | Ok cal -> process_ids (cal :: acc) rest
              | Error `Not_found -> process_ids acc rest
              | Error (`Msg e) -> Error (`Msg e))
        in
        let* calendar_names = process_ids [] ids in
        Ok (List.flatten calendar_names)
      with exn ->
        Error
          (`Msg
             (Printf.sprintf "Error getting calendar_names: %s"
                (Printexc.to_string exn))))

let add_event ~fs calendar_dir events event =
  let calendar_name = Event.get_calendar_name event in
  let file = Event.get_file event in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let calendar = Event.to_ical_calendar event in
  let content = Icalendar.to_ics ~cr:true calendar in
    try
      Eio.Path.save ~create:(`Or_truncate 0o644) file content;
      Ok (event :: events)
    with Eio.Exn.Io _ as exn ->
      Error
        (`Msg
           (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let edit_event ~fs calendar_dir events event =
  let calendar_name = Event.get_calendar_name event in
  let event_id = Event.get_id event in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let ical_event = Event.to_ical_event event in
  let file = Event.get_file event in
  let existing_props, existing_components = Event.to_ical_calendar event in
  let calendar =
    (* Replace the event with our updated version *)
    let filtered_components =
      List.filter
        (function
          | `Event e ->
              (* Filter out the old event *)
              let uid = e.Icalendar.uid in
              snd uid <> event_id
          | _ -> true)
        existing_components
    in
    (existing_props, `Event ical_event :: filtered_components)
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) file content;
    (* Filter out the old event and add the updated one *)
    let filtered_events = List.filter (fun e -> Event.get_id e <> event_id) events in
    Ok (event :: filtered_events)
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let delete_event ~fs calendar_dir events event =
  let calendar_name = Event.get_calendar_name event in
  let event_id = Event.get_id event in
  let calendar_name_path = get_calendar_path ~fs calendar_dir calendar_name in
  let* () = ensure_dir calendar_name_path in
  let file = Event.get_file event in
  let existing_props, existing_components = Event.to_ical_calendar event in
  let other_events = ref false in
  let calendar =
    (* Replace the event with our updated version *)
    let filtered_components =
      List.filter
        (function
          | `Event e ->
              (* Filter out the old event *)
              let uid = e.Icalendar.uid in
              if snd uid = event_id then false
              else (
                other_events := true;
                true)
          | _ -> true)
        existing_components
    in
    (existing_props, filtered_components)
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  try
    (match !other_events with
    | true -> Eio.Path.save ~create:(`Or_truncate 0o644) file content
    | false -> Eio.Path.unlink file);
    (* Filter out the deleted event from the events list *)
    let filtered_events = List.filter (fun e -> Event.get_id e <> event_id) events in
    Ok filtered_events
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to write file %s: %a\n%!" (snd file) Eio.Exn.pp exn))

let get_path t = t
