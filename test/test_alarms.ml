open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let make_display_alarm seconds =
  let span = Ptime.Span.of_int_s (- seconds) in
  let open Icalendar in
  `Display {
    trigger = (Params.empty, `Duration span);
    duration_repeat = None;
    summary = None;
    other = [];
    special = { description = None };
  }

let make_none_alarm () =
  let open Icalendar in
  `None {
    trigger = (Params.empty, `Duration (Ptime.Span.of_int_s (- 900)));
    duration_repeat = None;
    summary = None;
    other = [];
    special = ();
  }

let ptime_of ymd hms =
  Option.get @@ Ptime.of_date_time (ymd, (hms, 0))

let format_ptime t =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

(* --- Format function tests --- *)

let%expect_test "format_alarm_trigger" =
  let test span_secs =
    let span = Ptime.Span.of_int_s span_secs in
    Printf.printf "%d -> %s\n" span_secs (Format_utils.format_alarm_trigger span)
  in
  test 0;
  test (-900);      (* 15 min *)
  test (-3600);     (* 1 hour *)
  test (-86400);    (* 1 day *)
  test (-95400);    (* 1 day 2 hours 30 min = 86400 + 7200 + 1800 *)
  test 900;         (* 15 min after *)
  [%expect {|
    0 -> at start
    -900 -> 15 minutes before
    -3600 -> 1 hour before
    -86400 -> 1 day before
    -95400 -> 1 day 2 hours 30 minutes before
    900 -> 15 minutes after |}]

let%expect_test "format_alarm_short" =
  let test span_secs =
    let span = Ptime.Span.of_int_s span_secs in
    Printf.printf "%d -> %s\n" span_secs (Format_utils.format_alarm_short span)
  in
  test 0;
  test (-900);
  test (-3600);
  test (-86400);
  test (-95400);
  [%expect {|
    0 -> 0m
    -900 -> 15m
    -3600 -> 1h
    -86400 -> 1d
    -95400 -> 1d2h30m |}]

let%expect_test "format_alarms filters None alarms" =
  let alarms = [
    make_display_alarm 900;
    make_none_alarm ();
    make_display_alarm 3600;
  ] in
  Printf.printf "long: %s\n" (Format_utils.format_alarms alarms);
  Printf.printf "short: %s\n" (Format_utils.format_alarms_short alarms);
  [%expect {|
    long: 15 minutes before, 1 hour before
    short: 15m,1h |}]

let%expect_test "format_alarms empty list" =
  Printf.printf "long: '%s'\n" (Format_utils.format_alarms []);
  Printf.printf "short: '%s'\n" (Format_utils.format_alarms_short []);
  [%expect {|
    long: ''
    short: '' |}]

(* --- Event alarm fire computation tests --- *)

let%expect_test "event alarm fires from ics file" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  let events = List.filter_map Component.to_event components in
  let event = List.find (fun e -> Event.get_id e = "alarm-event@caledonia.test") events in
  let from = Some (ptime_of (2025, 4, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 30) (23, 59, 59) in
  let fires = Event.compute_alarm_fires ~from ~to_ event in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter (fun (af : Event.alarm_fire) ->
    Printf.printf "  %s\n" (format_ptime af.fire_time)
  ) fires;
  [%expect {|
    Number of alarm fires: 2
      2025-04-15T09:00:00Z
      2025-04-15T09:45:00Z |}]

let%expect_test "event with no alarms returns empty fires" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "example" in
  let events = List.filter_map Component.to_event components in
  let event = List.find (fun e -> Event.get_id e = "test-event@caledonia.test") events in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 1) (0, 0, 0) in
  let fires = Event.compute_alarm_fires ~from ~to_ event in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  [%expect {| Number of alarm fires: 0 |}]

let%expect_test "recurring event alarm fires" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  let events = List.filter_map Component.to_event components in
  let event = List.find (fun e -> Event.get_id e = "alarm-recurring@caledonia.test") events in
  (* Query 4 weeks starting from the event's first occurrence *)
  let from = Some (ptime_of (2025, 3, 27) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 24) (0, 0, 0) in
  let fires = Event.compute_alarm_fires ~from ~to_ event in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter (fun (af : Event.alarm_fire) ->
    Printf.printf "  %s\n" (format_ptime af.fire_time)
  ) fires;
  [%expect {|
    Number of alarm fires: 4
      2025-03-27T11:00:00Z
      2025-04-03T11:00:00Z
      2025-04-10T11:00:00Z
      2025-04-17T11:00:00Z |}]

(* --- Todo alarm fire computation tests --- *)

let%expect_test "todo alarm fires from ics file" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  let todos = List.filter_map Component.to_todo components in
  let todo = List.find (fun t -> Todo.get_id t = "alarm-todo@caledonia.test") todos in
  let from = Some (ptime_of (2025, 4, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 30) (23, 59, 59) in
  let fires = Todo.compute_alarm_fires ~from ~to_ todo in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter (fun (af : Todo.alarm_fire) ->
    Printf.printf "  %s\n" (format_ptime af.fire_time)
  ) fires;
  [%expect {|
    Number of alarm fires: 1
      2025-04-20T13:30:00Z |}]

(* --- Component unified alarm query tests --- *)

let%expect_test "component query_alarm_fires combines events and todos" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  (* Range that covers both the event (Apr 15) and todo (Apr 20) alarm fires *)
  let from = Some (ptime_of (2025, 4, 14) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 21) (0, 0, 0) in
  let fires = Component.query_alarm_fires ~from ~to_ components in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter (fun (af : Component.alarm_fire) ->
    let summary = match Component.get_summary af.component with
      | Some s -> s | None -> "(none)"
    in
    Printf.printf "  %s  %s\n" (format_ptime af.fire_time) summary
  ) fires;
  [%expect {|
    Number of alarm fires: 4
      2025-04-15T09:00:00Z  Alarm Test Event
      2025-04-15T09:45:00Z  Alarm Test Event
      2025-04-17T11:00:00Z  Recurring Alarm Event
      2025-04-20T13:30:00Z  Alarm Test Todo |}]

let%expect_test "component query_alarm_fires sorted by fire_time" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  let from = Some (ptime_of (2025, 3, 27) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 21) (0, 0, 0) in
  let fires = Component.query_alarm_fires ~from ~to_ components in
  (* Verify sorted order *)
  let times = List.map (fun (af : Component.alarm_fire) -> af.fire_time) fires in
  let sorted = List.for_all2 (fun a b -> Ptime.compare a b <= 0)
    (List.filteri (fun i _ -> i < List.length times - 1) times)
    (List.filteri (fun i _ -> i > 0) times)
  in
  Printf.printf "Results sorted by fire_time: %b\n" sorted;
  Printf.printf "Total fires: %d\n" (List.length fires);
  [%expect {|
    Results sorted by fire_time: true
    Total fires: 7 |}]
