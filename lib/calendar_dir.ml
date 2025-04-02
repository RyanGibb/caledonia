open Icalendar

module CollectionMap = Map.Make (struct
  type t = Collection.t

  let compare (Collection.Col a) (Collection.Col b) = String.compare a b
end)

type t = { path : string; mutable collections : Event.t list CollectionMap.t }

let get_collection_path ~fs calendar_dir (Collection.Col collection_name) =
  Eio.Path.(fs / calendar_dir.path / collection_name)

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
  | Ok () -> Ok { path; collections = CollectionMap.empty }
  | Error e -> Error e

let list_collections ~fs calendar_dir =
  try
    let dir = Eio.Path.(fs / calendar_dir.path) in
    let collections =
      Eio.Path.read_dir dir
      |> List.filter_map (fun file ->
             if Eio.Path.is_directory Eio.Path.(dir / file) then
               Some (Collection.Col file)
             else None)
      |> List.sort (fun (Collection.Col a) (Collection.Col b) ->
             String.compare a b)
    in
    Ok collections
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to list calendar directory %s: %a" calendar_dir.path
            Eio.Exn.pp exn))

let load_events collection collection_path file_name =
  let file = Eio.Path.(collection_path / file_name) in
  let _, file_path = file in
  match Filename.check_suffix file_name ".ics" with
  | false -> []
  | true -> (
      try
        let content = Eio.Path.load file in
        match parse content with
        | Ok calendar ->
            let events =
              List.filter_map
                (function
                  | `Event e ->
                      Some (Event.of_icalendar collection ~file_name e)
                  | _ -> None)
                (snd calendar)
            in
            events
        | Error err ->
            Printf.eprintf "Failed to parse %s: %s\n%!" file_path err;
            []
      with Eio.Exn.Io _ as exn ->
        Fmt.epr "Failed to read file %s: %a\n%!" file_path Eio.Exn.pp exn;
        [])

let get_collection ~fs calendar_dir collection =
  match CollectionMap.find_opt collection calendar_dir.collections with
  | Some events -> Ok events
  | None -> (
      let collection_path = get_collection_path ~fs calendar_dir collection in
      if not (Eio.Path.is_directory collection_path) then Error `Not_found
      else
        try
          let files = Eio.Path.read_dir collection_path in
          let events =
            List.flatten
            @@ List.map (load_events collection collection_path) files
          in
          calendar_dir.collections <-
            CollectionMap.add collection events calendar_dir.collections;
          Ok events
        with e ->
          Error
            (`Msg
               (Printf.sprintf "Exception processing directory %s: %s"
                  (snd collection_path) (Printexc.to_string e))))

let get_collections ~fs calendar_dir =
  match list_collections ~fs calendar_dir with
  | Error e -> Error e
  | Ok ids -> (
      try
        let rec process_ids acc = function
          | [] -> Ok (List.rev acc)
          | id :: rest -> (
              match get_collection ~fs calendar_dir id with
              | Ok cal -> process_ids ((id, cal) :: acc) rest
              | Error `Not_found -> process_ids acc rest
              | Error (`Msg e) -> Error (`Msg e))
        in
        process_ids [] ids
      with exn ->
        Error
          (`Msg
             (Printf.sprintf "Error getting collections: %s"
                (Printexc.to_string exn))))

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")

let add_event ~fs calendar_dir event =
  let collection = Event.get_collection event in
  let file_path =
    Event.get_file_path ~fs ~calendar_dir_path:calendar_dir.path event
  in
  let ical_event = Event.to_icalendar event in
  let collection_path = get_collection_path ~fs calendar_dir collection in
  let ( let* ) = Result.bind in
  let* () = ensure_dir collection_path in
  let calendar =
    let props = [ default_prodid ] in
    let components = [ `Event ical_event ] in
    (props, components)
  in
  let content = Icalendar.to_ics ~cr:true calendar in
  let* _ =
    try
      Eio.Path.save ~create:(`Or_truncate 0o644) file_path content;
      Ok ()
    with Eio.Exn.Io _ as exn ->
      Error
        (`Msg
           (Fmt.str "Failed to write file %s: %a\n%!" (snd file_path) Eio.Exn.pp
              exn))
  in
  calendar_dir.collections <-
    CollectionMap.add collection
      (event
      ::
      (match CollectionMap.find_opt collection calendar_dir.collections with
      | Some lst -> lst
      | None -> []))
      calendar_dir.collections;
  Ok ()

let load_calendar path =
  try
    let file_content = Eio.Path.load path in
    match Icalendar.parse file_content with
    | Ok calendar -> Ok calendar
    | Error msg -> Error (`Msg msg)
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg (Fmt.str "Failed to read file %s: %a\n%!" (snd path) Eio.Exn.pp exn))

let edit_event ~fs calendar_dir event =
  let collection = Event.get_collection event in
  let event_id = Event.get_id event in
  let collection_path = get_collection_path ~fs calendar_dir collection in
  let ( let* ) = Result.bind in
  let* () = ensure_dir collection_path in
  let ical_event = Event.to_icalendar event in
  let file_path =
    Event.get_file_path ~fs ~calendar_dir_path:calendar_dir.path event
  in
  let* existing_props, existing_components = load_calendar file_path in
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
  let* _ =
    try
      Eio.Path.save ~create:(`Or_truncate 0o644) file_path content;
      Ok ()
    with Eio.Exn.Io _ as exn ->
      Error
        (`Msg
           (Fmt.str "Failed to write file %s: %a\n%!" (snd file_path) Eio.Exn.pp
              exn))
  in
  calendar_dir.collections <-
    CollectionMap.add collection
      (event
      ::
      (match CollectionMap.find_opt collection calendar_dir.collections with
      (* filter old version *)
      | Some lst -> List.filter (fun e -> Event.get_id e = event_id) lst
      | None -> []))
      calendar_dir.collections;
  Ok ()

let delete_event ~fs calendar_dir event =
  let collection = Event.get_collection event in
  let event_id = Event.get_id event in
  let collection_path = get_collection_path ~fs calendar_dir collection in
  let ( let* ) = Result.bind in
  let* () = ensure_dir collection_path in
  let file_path =
    Event.get_file_path ~fs ~calendar_dir_path:calendar_dir.path event
  in
  let* existing_props, existing_components = load_calendar file_path in
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
  let* _ =
    try
      (match !other_events with
      | true -> Eio.Path.save ~create:(`Or_truncate 0o644) file_path content
      | false -> Eio.Path.unlink file_path);
      Ok ()
    with Eio.Exn.Io _ as exn ->
      Error
        (`Msg
           (Fmt.str "Failed to write file %s: %a\n%!" (snd file_path) Eio.Exn.pp
              exn))
  in
  calendar_dir.collections <-
    CollectionMap.add collection
      (match CollectionMap.find_opt collection calendar_dir.collections with
      (* filter old version *)
      | Some lst -> List.filter (fun e -> Event.get_id e = event_id) lst
      | None -> [])
      calendar_dir.collections;
  Ok ()

let get_path t = t.path
