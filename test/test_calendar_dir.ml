open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let%expect_test "list calendar names" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let calendar_names = Result.get_ok @@ Calendar_dir.list_calendar_names ~fs calendar_dir in
  Printf.printf "Number of calendars: %d\n" (List.length calendar_names);
  Printf.printf "Contains 'example': %b\n" (List.exists (fun c -> c = "example") calendar_names);
  Printf.printf "Contains 'recurrence': %b\n" (List.exists (fun c -> c = "recurrence") calendar_names);
  [%expect {|
    Number of calendars: 3
    Contains 'example': true
    Contains 'recurrence': true |}]

let%expect_test "get calendar events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let result = Calendar_dir.get_calendar_events ~fs calendar_dir "example" in
  Printf.printf "Found 'example' calendar: %b\n" (Result.is_ok result);
  [%expect {| Found 'example' calendar: true |}]

let%expect_test "get all events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  Printf.printf "Total events: %d\n" (List.length events);
  [%expect {| Total events: 35 |}]
