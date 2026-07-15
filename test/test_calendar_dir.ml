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

let start_ts =
  ( Icalendar.Params.empty,
    `Datetime (`Utc (Option.get @@ Ptime.of_date_time ((2025, 4, 17), ((13, 0, 0), 0)))) )

let%expect_test "add, edit, and delete an event on disk" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_dir "caledonia_test" "" in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp in

  (* add: file appears in the calendar subdirectory *)
  let event =
    Result.get_ok
    @@ Event.create ~fs ~calendar_dir_path:tmp ~summary:"Dentist"
         ~start:start_ts "personal"
  in
  let events = Result.get_ok @@ Calendar_dir.add_event ~fs calendar_dir [] event in
  Printf.printf "events after add: %d\n" (List.length events);
  let on_disk = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  Printf.printf "on disk after add: %d\n" (List.length on_disk);
  Printf.printf "ics files: %d\n"
    (List.length (Sys.readdir (Filename.concat tmp "personal") |> Array.to_list));

  (* edit: same file, updated summary *)
  let edited = Result.get_ok @@ Event.edit ~summary:"Dentist (moved)" event in
  let events = Result.get_ok @@ Calendar_dir.edit_event ~fs calendar_dir events edited in
  Printf.printf "events after edit: %d\n" (List.length events);
  let on_disk = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  (match on_disk with
  | [ e ] -> Printf.printf "summary on disk: %s\n" (Option.get (Event.get_summary e))
  | l -> Printf.printf "unexpected event count: %d\n" (List.length l));

  (* delete: file is removed *)
  let events = Result.get_ok @@ Calendar_dir.delete_event ~fs calendar_dir events edited in
  Printf.printf "events after delete: %d\n" (List.length events);
  Printf.printf "ics files after delete: %d\n"
    (Array.length (Sys.readdir (Filename.concat tmp "personal")));
  [%expect {|
    events after add: 1
    on disk after add: 1
    ics files: 1
    events after edit: 1
    summary on disk: Dentist (moved)
    events after delete: 0
    ics files after delete: 0
    |}]

let%expect_test "display name and color fall back when files are absent" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_dir "caledonia_test" "" in
  Unix.mkdir (Filename.concat tmp "work") 0o755;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp in
  Printf.printf "display name fallback: %s\n"
    (Calendar_dir.get_display_name ~fs calendar_dir "work");
  Printf.printf "color fallback: %s\n"
    (match Calendar_dir.get_color ~fs calendar_dir "work" with
    | Some c -> c
    | None -> "none");
  let oc = open_out (Filename.concat tmp "work/displayname") in
  output_string oc "Work Calendar\n";
  close_out oc;
  let oc = open_out (Filename.concat tmp "work/color") in
  output_string oc "#ff0000\n";
  close_out oc;
  Printf.printf "display name: %s\n"
    (Calendar_dir.get_display_name ~fs calendar_dir "work");
  Printf.printf "color: %s\n"
    (match Calendar_dir.get_color ~fs calendar_dir "work" with
    | Some c -> c
    | None -> "none");
  Printf.printf "found by display name: %s\n"
    (match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir "Work Calendar" with
    | Some dir -> dir
    | None -> "not found");
  [%expect {|
    display name fallback: work
    color fallback: none
    display name: Work Calendar
    color: #ff0000
    found by display name: work
    |}]
