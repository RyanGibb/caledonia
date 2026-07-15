open Caledonia_lib

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let ptime_of ymd hms =
  Option.get @@ Ptime.of_date_time (ymd, (hms, 0))

(* --- sexp_of_t includes start_utc --- *)

let%expect_test "sexp_of_t includes start_utc" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event =
    List.find
      (fun e -> Event.get_summary e = Some "Test Event")
      events
  in
  let sexp = Event.sexp_of_t event in
  let sexp_str = Sexplib.Sexp.to_string_hum sexp in
  (* Check that start_utc is present *)
  Printf.printf "has start_utc: %b\n"
    (try ignore (Str.search_forward (Str.regexp "start_utc") sexp_str 0); true
     with Not_found -> false);
  (* Check it looks like an RFC 3339 timestamp *)
  Printf.printf "has rfc3339 format: %b\n"
    (try ignore (Str.search_forward (Str.regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T") sexp_str 0); true
     with Not_found -> false);
  [%expect {|
    has start_utc: true
    has rfc3339 format: true |}]

(* --- sexp_of_t uses event timezone for start/end --- *)

let%expect_test "sexp_of_t formats times in event timezone not local" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event =
    List.find
      (fun e -> Event.get_summary e = Some "Timezone Test Event")
      events
  in
  let sexp = Event.sexp_of_t event in
  let sexp_str = Sexplib.Sexp.to_string_hum sexp in
  (* start should be 19:30 (Asia/Kolkata), not 14:00 (UTC) or 15:00 (BST) *)
  Printf.printf "has start 19:30: %b\n"
    (try ignore (Str.search_forward (Str.regexp "19:30:00") sexp_str 0); true
     with Not_found -> false);
  (* end should be 21:30 (Asia/Kolkata) *)
  Printf.printf "has end 21:30: %b\n"
    (try ignore (Str.search_forward (Str.regexp "21:30:00") sexp_str 0); true
     with Not_found -> false);
  (* start_tz should be Asia/Kolkata *)
  Printf.printf "has start_tz Kolkata: %b\n"
    (try ignore (Str.search_forward (Str.regexp "Asia/Kolkata") sexp_str 0); true
     with Not_found -> false);
  [%expect {|
    has start 19:30: true
    has end 21:30: true
    has start_tz Kolkata: true |}]

(* --- delete_occurrence adds EXDATE --- *)

let%expect_test "delete_occurrence adds EXDATE to recurring event" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  (* Find the weekly recurring event *)
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  (* Expand before deletion *)
  let before = Event.expand_recurrences ~from ~to_ weekly in
  let count_before = List.length before in
  Printf.printf "occurrences before: %d\n" count_before;
  (* Pick the second occurrence's start time to delete *)
  let second_occ = List.nth before 1 in
  let occ_start = Event.get_start second_occ in
  Printf.printf "deleting occurrence at: %s\n" (Ptime.to_rfc3339 occ_start);
  (* Delete that occurrence *)
  let modified = Event.delete_occurrence weekly occ_start in
  let after = Event.expand_recurrences ~from ~to_ modified in
  let count_after = List.length after in
  Printf.printf "occurrences after: %d\n" count_after;
  Printf.printf "one fewer: %b\n" (count_after = count_before - 1);
  (* Verify the deleted occurrence is gone *)
  let still_has =
    List.exists
      (fun e -> Ptime.equal (Event.get_start e) occ_start)
      after
  in
  Printf.printf "deleted occurrence still present: %b\n" still_has;
  [%expect {|
    occurrences before: 10
    deleting occurrence at: 2025-04-03T12:00:00-00:00
    occurrences after: 9
    one fewer: true
    deleted occurrence still present: false
    |}]

(* --- delete_occurrence on existing exdate event --- *)

let%expect_test "delete_occurrence with specific known timestamp" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let before = Event.expand_recurrences ~from ~to_ weekly in
  Printf.printf "occurrences before: %d\n" (List.length before);
  (* Delete using a manually constructed timestamp for Apr 17 12:00 UTC *)
  let occ_to_delete = ptime_of (2025, 4, 17) (12, 0, 0) in
  (* Verify this occurrence exists *)
  let exists_before =
    List.exists (fun e -> Ptime.equal (Event.get_start e) occ_to_delete) before
  in
  Printf.printf "occurrence exists before: %b\n" exists_before;
  let modified = Event.delete_occurrence weekly occ_to_delete in
  let after = Event.expand_recurrences ~from ~to_ modified in
  Printf.printf "occurrences after: %d\n" (List.length after);
  let exists_after =
    List.exists (fun e -> Ptime.equal (Event.get_start e) occ_to_delete) after
  in
  Printf.printf "occurrence exists after: %b\n" exists_after;
  [%expect {|
    occurrences before: 10
    occurrence exists before: true
    occurrences after: 9
    occurrence exists after: false
    |}]

(* --- create_occurrence_override --- *)

let%expect_test "create_occurrence_override has correct structure" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  let override =
    Event.create_occurrence_override weekly occ_start
      ~summary:"Modified Weekly" ~location:"New Room" ()
  in
  (* Check UID matches parent *)
  Printf.printf "uid matches: %b\n"
    (snd override.Icalendar.uid = Event.get_id weekly);
  (* Check no RRULE *)
  Printf.printf "no rrule: %b\n" (override.Icalendar.rrule = None);
  (* Check has RECURRENCE-ID *)
  let has_recur_id =
    List.exists
      (function `Recur_id _ -> true | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has recurrence-id: %b\n" has_recur_id;
  (* Check summary was overridden *)
  let has_summary =
    List.exists
      (function `Summary (_, s) -> s = "Modified Weekly" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has overridden summary: %b\n" has_summary;
  (* Check location was overridden *)
  let has_location =
    List.exists
      (function `Location (_, l) -> l = "New Room" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has overridden location: %b\n" has_location;
  [%expect {|
    uid matches: true
    no rrule: true
    has recurrence-id: true
    has overridden summary: true
    has overridden location: true |}]

let%expect_test "create_occurrence_override inherits unmodified props" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  (* Only change location, leave summary untouched *)
  let override =
    Event.create_occurrence_override weekly occ_start
      ~location:"New Room" ()
  in
  (* Summary should be inherited from parent *)
  let has_parent_summary =
    List.exists
      (function `Summary (_, s) -> s = "Weekly Recurring Event" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "inherited parent summary: %b\n" has_parent_summary;
  [%expect {| inherited parent summary: true |}]

(* --- add_occurrence_override preserves parent series --- *)

let%expect_test "add_occurrence_override preserves recurring series" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  (* Set up a temp calendar dir with a recurring event *)
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:override-test@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250327T120000Z\r\n\
     DTEND:20250327T130000Z\r\n\
     SUMMARY:Weekly Meeting\r\n\
     LOCATION:Room A\r\n\
     RRULE:FREQ=WEEKLY\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics_path = Filename.concat cal_path "override-test.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event = List.hd events in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = Event.query events ~from ~to_ () in
  Printf.printf "occurrences before override: %d\n" (List.length before);
  (* Create an override for the second occurrence (Apr 3) *)
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  let override = Event.create_occurrence_override event occ_start
      ~summary:"Modified Meeting" ~location:"Room B" () in
  let events_after = Result.get_ok @@
    Calendar_dir.add_occurrence_override ~fs calendar_dir events event override in
  let after = Event.query events_after ~from ~to_ () in
  Printf.printf "occurrences after override: %d\n" (List.length after);
  (* The overridden occurrence should have the new summary *)
  let modified_occ =
    List.find_opt (fun e -> Event.get_summary e = Some "Modified Meeting") after
  in
  Printf.printf "override occurrence found: %b\n" (modified_occ <> None);
  (* Other occurrences should still have the original summary *)
  let original_occs =
    List.filter (fun e -> Event.get_summary e = Some "Weekly Meeting") after
  in
  Printf.printf "original occurrences remaining: %d\n" (List.length original_occs);
  Printf.printf "series preserved: %b\n" (List.length after = List.length before);
  (* Clean up *)
  Sys.remove ics_path;
  Sys.rmdir cal_path;
  Sys.rmdir tmp_dir;
  [%expect {|
    occurrences before override: 10
    occurrences after override: 10
    override occurrence found: true
    original occurrences remaining: 9
    series preserved: true
    |}]

(* --- delete_occurrence with timezone-aware DTSTART --- *)

let%expect_test "two consecutive delete_occurrences with TZID" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    String.concat "\r\n" [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Test//EN";
      "BEGIN:VEVENT";
      "UID:two-deletes@caledonia.test";
      "DTSTAMP:20260312T000000Z";
      "DTSTART;TZID=Europe/London:20260312T060000";
      "DTEND;TZID=Europe/London:20260312T070000";
      "RRULE:FREQ=DAILY;COUNT=7";
      "SUMMARY:test";
      "END:VEVENT";
      "END:VCALENDAR";
      ""
    ]
  in
  let ics_path = Filename.concat cal_path "two-deletes.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event = List.hd events in
  let from = Some (ptime_of (2026, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2026, 3, 31) (23, 59, 59) in
  let before = Event.query events ~from ~to_ () in
  Printf.printf "before: %d\n" (List.length before);
  (* Delete the 15th — get_start returns UTC, London is GMT in March *)
  let occ_15 = Event.get_start (List.nth before 3) in
  let _events = Result.get_ok @@
    Calendar_dir.delete_occurrence ~fs calendar_dir events event occ_15 in
  (* Simulate server Refresh: re-read from disk *)
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event = List.hd events in
  let mid = Event.query events ~from ~to_ () in
  Printf.printf "after first delete: %d\n" (List.length mid);
  (* Delete the 14th *)
  let occ_14 = Event.get_start (List.nth mid 2) in
  let _events = Result.get_ok @@
    Calendar_dir.delete_occurrence ~fs calendar_dir events event occ_14 in
  (* Re-read from disk again *)
  let events_final = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let after = Event.query events_final ~from ~to_ () in
  Printf.printf "after second delete: %d\n" (List.length after);
  List.iter (fun e ->
    let (y, m, d), ((hh, mm, _ss), _) = Ptime.to_date_time (Event.get_start e) in
    Printf.printf "  %04d-%02d-%02d %02d:%02d\n" y m d hh mm
  ) after;
  (* Clean up *)
  Sys.remove ics_path;
  Sys.rmdir cal_path;
  Sys.rmdir tmp_dir;
  [%expect {|
    before: 7
    after first delete: 6
    after second delete: 5
      2026-03-12 06:00
      2026-03-13 06:00
      2026-03-16 06:00
      2026-03-17 06:00
      2026-03-18 06:00
    |}]

let%expect_test "delete_occurrence works with TZID dtstart" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    String.concat "\r\n" [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Test//EN";
      "BEGIN:VEVENT";
      "UID:tz-delete@caledonia.test";
      "DTSTAMP:20250327T000000Z";
      "DTSTART;TZID=Europe/London:20250327T120000";
      "DTEND;TZID=Europe/London:20250327T130000";
      "SUMMARY:TZ Weekly";
      "RRULE:FREQ=WEEKLY;COUNT=5";
      "END:VEVENT";
      "END:VCALENDAR";
      ""
    ]
  in
  let ics_path = Filename.concat cal_path "tz-delete.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event = List.hd events in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = Event.query events ~from ~to_ () in
  Printf.printf "occurrences before: %d\n" (List.length before);
  (* Delete the second occurrence *)
  let second_occ = List.nth before 1 in
  let occ_start = Event.get_start second_occ in
  Printf.printf "deleting: %s\n" (Ptime.to_rfc3339 occ_start);
  let _events_after = Result.get_ok @@
    Calendar_dir.delete_occurrence ~fs calendar_dir events event occ_start in
  (* Re-read from disk to verify EXDATE is honored *)
  let events_disk = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let after = Event.query events_disk ~from ~to_ () in
  Printf.printf "occurrences after (from disk): %d\n" (List.length after);
  Printf.printf "one fewer: %b\n" (List.length after = List.length before - 1);
  (* Clean up *)
  Sys.remove ics_path;
  Sys.rmdir cal_path;
  Sys.rmdir tmp_dir;
  [%expect {|
    occurrences before: 5
    deleting: 2025-04-03T11:00:00-00:00
    occurrences after (from disk): 4
    one fewer: true
    |}]

(* --- delete_occurrence preserves existing overrides --- *)

let%expect_test "delete_occurrence preserves existing RECURRENCE-ID overrides" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  (* Set up a temp calendar dir with a recurring event + override *)
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:preserve-override@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250327T120000Z\r\n\
     DTEND:20250327T130000Z\r\n\
     SUMMARY:Weekly Meeting\r\n\
     RRULE:FREQ=WEEKLY\r\n\
     END:VEVENT\r\n\
     BEGIN:VEVENT\r\n\
     UID:preserve-override@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250403T140000Z\r\n\
     DTEND:20250403T150000Z\r\n\
     SUMMARY:Modified Meeting\r\n\
     RECURRENCE-ID:20250403T120000Z\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics_path = Filename.concat cal_path "preserve-override.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let event = List.hd events in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = Event.query events ~from ~to_ () in
  Printf.printf "occurrences before: %d\n" (List.length before);
  let has_override_before =
    List.exists (fun e -> Event.get_summary e = Some "Modified Meeting") before
  in
  Printf.printf "has override before: %b\n" has_override_before;
  (* Delete a different occurrence (Apr 10) — should NOT remove the override *)
  let occ_to_delete = ptime_of (2025, 4, 10) (12, 0, 0) in
  let _events_after = Result.get_ok @@
    Calendar_dir.delete_occurrence ~fs calendar_dir events event occ_to_delete in
  (* Re-read from disk to verify the file preserved the override *)
  let events_from_disk = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let after = Event.query events_from_disk ~from ~to_ () in
  Printf.printf "occurrences after: %d\n" (List.length after);
  let has_override_after =
    List.exists (fun e -> Event.get_summary e = Some "Modified Meeting") after
  in
  Printf.printf "has override after: %b\n" has_override_after;
  (* Clean up *)
  Sys.remove ics_path;
  Sys.rmdir cal_path;
  Sys.rmdir tmp_dir;
  [%expect {|
    occurrences before: 10
    has override before: true
    occurrences after: 9
    has override after: true
    |}]

(* --- Alarm sexp round-trip --- *)

let%expect_test "alarm sexp output uses short parseable format" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let components = Result.get_ok @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm" in
  let events = List.filter_map Component.to_event components in
  let event = List.find (fun e -> Event.get_id e = "alarm-event@caledonia.test") events in
  let sexp = Event.sexp_of_t event in
  (* Extract the alarms value from the sexp *)
  let alarm_str = match sexp with
    | Sexplib.Sexp.List fields ->
        (match List.find_opt (function
          | Sexplib.Sexp.List (Sexplib.Sexp.Atom "alarms" :: _) -> true
          | _ -> false) fields with
        | Some (Sexplib.Sexp.List [_; Sexplib.Sexp.Atom s]) -> s
        | _ -> "")
    | _ -> ""
  in
  Printf.printf "alarm sexp value: %s\n" alarm_str;
  (* Should be short format like "1h,15m" not "1 hour before, 15 minutes before" *)
  Printf.printf "uses short format: %b\n"
    (not (String.contains alarm_str ' '));
  [%expect {|
    alarm sexp value: 1h,15m
    uses short format: true
    |}]

(* --- Sexp protocol parsing --- *)

let%expect_test "delete_event_request parses with occurrence_start" =
  let sexp = Sexplib.Sexp.of_string {|((id "abc-123")(occurrence_start "2025-04-03T12:00:00Z"))|} in
  let req = Sexp.delete_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect {|
    id: abc-123
    occurrence_start: 2025-04-03T12:00:00Z |}]

let%expect_test "delete_event_request parses without occurrence_start" =
  let sexp = Sexplib.Sexp.of_string {|((id "abc-123"))|} in
  let req = Sexp.delete_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect {|
    id: abc-123
    occurrence_start: none |}]

let%expect_test "edit_event_request parses with occurrence_start" =
  let sexp = Sexplib.Sexp.of_string {|((id "abc-123")(summary "New Title")(occurrence_start "2025-04-03T12:00:00Z"))|} in
  let req = Sexp.edit_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "summary: %s\n"
    (match req.summary with Some s -> s | None -> "none");
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect {|
    id: abc-123
    summary: New Title
    occurrence_start: 2025-04-03T12:00:00Z |}]

let%expect_test "DeleteEvent request parses with new record format" =
  let sexp = Sexplib.Sexp.of_string {|(DeleteEvent ((id "abc-123")))|} in
  let req = Sexp.request_of_sexp sexp in
  (match req with
  | Sexp.DeleteEvent r ->
      Printf.printf "id: %s\n" r.id;
      Printf.printf "occurrence_start: %s\n"
        (match r.occurrence_start with Some s -> s | None -> "none")
  | _ -> Printf.printf "wrong variant\n");
  [%expect {|
    id: abc-123
    occurrence_start: none |}]

let%expect_test "DeleteEvent request parses with occurrence_start" =
  let sexp = Sexplib.Sexp.of_string {|(DeleteEvent ((id "abc-123")(occurrence_start "2025-04-03T12:00:00Z")))|} in
  let req = Sexp.request_of_sexp sexp in
  (match req with
  | Sexp.DeleteEvent r ->
      Printf.printf "id: %s\n" r.id;
      Printf.printf "occurrence_start: %s\n"
        (match r.occurrence_start with Some s -> s | None -> "none")
  | _ -> Printf.printf "wrong variant\n");
  [%expect {|
    id: abc-123
    occurrence_start: 2025-04-03T12:00:00Z |}]
