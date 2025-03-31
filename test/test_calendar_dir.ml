(* Test the Calendar_dir.module *)

open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let test_list_collections ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  match Calendar_dir.list_collections ~fs calendar_dir with
  | Error (`Msg msg) -> Alcotest.fail ("List collections failed: " ^ msg)
  | Ok collections ->
      Alcotest.(check int)
        "Should find two collections" 2 (List.length collections);
      Alcotest.(check bool)
        "example should be in the list" true
        (List.exists (fun c -> c = Collection.Col "example") collections);
      Alcotest.(check bool)
        "recurrence should be in the list" true
        (List.exists (fun c -> c = Collection.Col "recurrence") collections);
      ()

let test_get_collection ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  let result =
    Calendar_dir.get_collection ~fs calendar_dir (Collection.Col "example")
  in
  match result with
  | Ok _ -> Alcotest.(check pass) "Should find example collection" () ()
  | Error `Not_found -> Alcotest.fail "Failed to find example collection"
  | Error (`Msg msg) -> Alcotest.fail ("Error getting collection: " ^ msg)

let test_get_collections ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  match Calendar_dir.get_collections ~fs calendar_dir with
  | Ok collections ->
      Alcotest.(check int)
        "Should find two collections" 2 (List.length collections);
      Alcotest.(check bool)
        "example should be in the results" true
        (List.exists (fun (id, _) -> id = Collection.Col "example") collections);
      Alcotest.(check bool)
        "recurrence should be in the results" true
        (List.exists
           (fun (id, _) -> id = Collection.Col "recurrence")
           collections);
      ()
  | Error e ->
      let msg =
        match e with `Msg m -> m | `Not_found -> "Collection not found"
      in
      Alcotest.fail ("Error getting collections: " ^ msg)

let calendar_tests fs =
  [
    ("list collections", `Quick, test_list_collections ~fs);
    ("get collection", `Quick, test_get_collection ~fs);
    ("get all collections", `Quick, test_get_collections ~fs);
  ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Alcotest.run "Calendar_dir.Tests" [ ("calendar", calendar_tests fs) ]
