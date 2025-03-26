open Icalendar

type collection = Collection of string
type calendar_file = { calendar : calendar; file_path : string }

module CollectionMap = Map.Make (struct
  type t = collection

  let compare (Collection a) (Collection b) = String.compare a b
end)

type calendar_dir = {
  path : string;
  mutable collections : calendar_file list CollectionMap.t;
}

let collection_path calendar_dir (Collection collection_name) =
  Filename.concat calendar_dir.path collection_name

let ensure_dir ~fs path =
  try
    if not (Sys.file_exists path) then (
      Eio.Path.(mkdir ~perm:0o755 (fs / path));
      Ok ())
    else if not (Sys.is_directory path) then
      Error (`Msg (Printf.sprintf "%s exists but is not a directory" path))
    else Ok ()
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg (Fmt.str "Failed to create directory %s: %a" path Eio.Exn.pp exn))

let create ~fs path =
  match ensure_dir ~fs path with
  | Ok () -> Ok { path; collections = CollectionMap.empty }
  | Error e -> Error e

let list_collections ~fs calendar_dir =
  try
    let dir_path = Eio.Path.(fs / calendar_dir.path) in
    let collections =
      Eio.Path.read_dir dir_path
      |> List.filter_map (fun file ->
             let path = Filename.concat calendar_dir.path file in
             if Sys.is_directory path then Some (Collection file) else None)
      |> List.sort (fun (Collection a) (Collection b) -> String.compare a b)
    in
    Ok collections
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to list calendar directory %s: %a" calendar_dir.path
            Eio.Exn.pp exn))

let get_collection ~fs calendar_dir collection =
  match CollectionMap.find_opt collection calendar_dir.collections with
  | Some calendars -> Ok calendars
  | None -> (
      let collection_path = collection_path calendar_dir collection in
      if not (Sys.is_directory collection_path) then Error `Not_found
      else
        try
          let files = Sys.readdir collection_path in
          let calendar_files =
            List.filter_map
              (fun filename ->
                match Filename.check_suffix filename ".ics" with
                | false -> None
                | true -> (
                    let file = Eio.Path.(fs / collection_path / filename) in
                    let _, file_path = file in
                    try
                      let content = Eio.Path.load file in
                      match parse content with
                      | Ok calendar -> Some { calendar; file_path }
                      | Error err ->
                          Printf.eprintf "Failed to parse %s: %s\n%!" file_path
                            err;
                          None
                    with Eio.Exn.Io _ as exn ->
                      Fmt.epr "Failed to read file %s: %a\n%!" file_path
                        Eio.Exn.pp exn;
                      None))
              (Array.to_list files)
          in
          calendar_dir.collections <-
            CollectionMap.add collection calendar_files calendar_dir.collections;
          Ok calendar_files
        with e ->
          Error
            (`Msg
               (Printf.sprintf "Exception processing directory %s: %s"
                  collection_path (Printexc.to_string e))))

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
