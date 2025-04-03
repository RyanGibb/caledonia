(* Test the Calendar_dir.module *)

open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let test_list_calendar_names ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  match Calendar_dir.list_calendar_names ~fs calendar_dir with
  | Error (`Msg msg) -> Alcotest.fail ("List calendar_names failed: " ^ msg)
  | Ok calendar_names ->
      Alcotest.(check int)
        "Should find two calendar_names" 2
        (List.length calendar_names);
      Alcotest.(check bool)
        "example should be in the list" true
        (List.exists (fun c -> c = "example") calendar_names);
      Alcotest.(check bool)
        "recurrence should be in the list" true
        (List.exists (fun c -> c = "recurrence") calendar_names);
      ()

let test_get_calendar_events ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  let result = Calendar_dir.get_calendar_events ~fs calendar_dir "example" in
  match result with
  | Ok _ -> Alcotest.(check pass) "Should find example calendar_name" () ()
  | Error `Not_found -> Alcotest.fail "Failed to find example calendar_name"
  | Error (`Msg msg) -> Alcotest.fail ("Error getting calendar_name: " ^ msg)

let test_get_events ~fs () =
  let calendar_dir =
    match Calendar_dir.create ~fs calendar_dir_path with
    | Ok dir -> dir
    | Error (`Msg msg) ->
        Alcotest.fail ("Calendar directory creation failed: " ^ msg)
  in
  match Calendar_dir.get_events ~fs calendar_dir with
  | Ok events ->
      Alcotest.(check int) "Should find two events" 32 (List.length events);
      ()
  | Error e ->
      let msg = match e with `Msg m -> m in
      Alcotest.fail ("Error getting calendar_names: " ^ msg)

let calendar_tests fs =
  [
    ("list calendar_names", `Quick, test_list_calendar_names ~fs);
    ("get calendar_name", `Quick, test_get_calendar_events ~fs);
    ("get all calendar_names", `Quick, test_get_events ~fs);
  ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Alcotest.run "Calendar_dir.Tests" [ ("calendar", calendar_tests fs) ]
