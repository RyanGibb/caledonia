open Caledonia_lib

let utc = Timedesc.Time_zone.utc

let parse_event ~fs ics =
  let calendar = Result.get_ok @@ Icalendar.parse ics in
  let file = Eio.Path.(fs / "test.ics") in
  List.hd (Event.events_of_icalendar "work" ~file calendar)

let timed_event_ics =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Test//Test//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:timed-event\r\n\
   DTSTAMP:20250101T000000Z\r\n\
   DTSTART;TZID=Europe/London:20250417T140000\r\n\
   DTEND;TZID=Europe/London:20250417T150000\r\n\
   SUMMARY:Team meeting\r\n\
   LOCATION:Room 3\r\n\
   DESCRIPTION:Quarterly planning\r\n\
   END:VEVENT\r\n\
   END:VCALENDAR\r\n"

let all_day_event_ics =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Test//Test//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:all-day-event\r\n\
   DTSTAMP:20250101T000000Z\r\n\
   DTSTART;VALUE=DATE:20250417\r\n\
   DTEND;VALUE=DATE:20250419\r\n\
   SUMMARY:Conference\r\n\
   END:VEVENT\r\n\
   END:VCALENDAR\r\n"

let recurring_event_ics =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Test//Test//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:recurring-event\r\n\
   DTSTAMP:20250101T000000Z\r\n\
   DTSTART;TZID=Europe/London:20250417T090000\r\n\
   DTEND;TZID=Europe/London:20250417T093000\r\n\
   RRULE:FREQ=WEEKLY;UNTIL=20250601T080000Z\r\n\
   SUMMARY:Standup\r\n\
   END:VEVENT\r\n\
   END:VCALENDAR\r\n"

let%expect_test "format_event text" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  (* All-day events are parsed as midnight in the process timezone, so pin it *)
  Unix.putenv "TZ" "UTC";
  print_endline (Event.format_event ~format:`Text ~tz:utc (parse_event ~fs timed_event_ics));
  print_endline (Event.format_event ~format:`Text ~tz:utc (parse_event ~fs all_day_event_ics));
  [%expect {|
    work	2025-04-17 Thu 13:00 - 14:00 (Europe/London)	Team meeting	@Room 3	timed-event
    work	2025-04-17 Thu - 2025-04-19 Sat	Conference		all-day-event
    |}]

let%expect_test "format_event entries" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  print_string (Event.format_event ~format:`Entries ~tz:utc (parse_event ~fs timed_event_ics));
  [%expect {|
    Summary: Team meeting
    Start: 2025-04-17 Thu 13:00(UTC)
    End: 2025-04-17 Thu 14:00(UTC) (Europe/London)
    Location: Room 3
    Description: Quarterly planning
    File: test.ics
    |}]

let%expect_test "format_event entries shows recurrence with UTC UNTIL" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  print_string (Event.format_event ~format:`Entries ~tz:utc (parse_event ~fs recurring_event_ics));
  [%expect {|
    Summary: Standup
    Start: 2025-04-17 Thu 08:00(UTC)
    End: 2025-04-17 Thu 08:30(UTC) (Europe/London)
    Recurrence: FREQ=WEEKLY;UNTIL=20250601T080000Z
    File: test.ics
    |}]

let%expect_test "format_event json and csv" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  print_endline (Event.format_event ~format:`Json ~tz:utc (parse_event ~fs timed_event_ics));
  print_endline (Event.format_event ~format:`Csv ~tz:utc (parse_event ~fs timed_event_ics));
  [%expect {|
    {"id":"timed-event","summary":"Team meeting","start":"2025-04-17 Thu 13:00(UTC)","end":"2025-04-17 Thu 14:00(UTC)","location":"Room 3","description":"Quarterly planning","calendar":"work","alarms":[]}
    "Team meeting","2025-04-17 Thu 13:00(UTC)","2025-04-17 Thu 14:00(UTC)","Room 3","work"
    |}]

let%expect_test "format_event sexp matches sexp_of_t" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Unix.putenv "TZ" "UTC";
  let event = parse_event ~fs timed_event_ics in
  let via_format = Event.format_event ~format:`Sexp ~tz:utc event in
  let via_sexp_of_t = Sexplib.Sexp.to_string (Event.sexp_of_t event) in
  Printf.printf "identical: %b\n" (String.equal via_format via_sexp_of_t);
  print_endline via_format;
  [%expect {|
    identical: true
    ((id timed-event)(summary"Team meeting")(start 2025-04-17T14:00:00)(start_local 2025-04-17T13:00:00)(start_tz Europe/London)(end 2025-04-17T15:00:00)(end_local 2025-04-17T14:00:00)(end_tz Europe/London)(location"Room 3")(description"Quarterly planning")(start_utc 2025-04-17T13:00:00-00:00)(file test.ics)(calendar work))
    |}]

let%expect_test "all-day events display their civil date in any timezone" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  (* Parse under BST so local midnight differs from UTC midnight *)
  Unix.putenv "TZ" "Europe/London";
  let event = parse_event ~fs all_day_event_ics in
  let auckland = Timedesc.Time_zone.make_exn "Pacific/Auckland" in
  print_endline (Event.format_event ~format:`Text ~tz:utc event);
  print_endline (Event.format_event ~format:`Text ~tz:auckland event);
  print_endline (Event.format_event ~format:`Json ~tz:utc event);
  print_endline (Event.format_event ~format:`Csv ~tz:utc event);
  print_string (Event.format_event ~format:`Entries ~tz:utc event);
  [%expect {|
    work	2025-04-17 Thu - 2025-04-19 Sat	Conference		all-day-event
    work	2025-04-17 Thu - 2025-04-19 Sat	Conference		all-day-event
    {"id":"all-day-event","summary":"Conference","start":"2025-04-17 Thu","end":"2025-04-19 Sat","location":null,"description":null,"calendar":"work","alarms":[]}
    "Conference","2025-04-17 Thu","2025-04-19 Sat","","work"
    Summary: Conference
    Start: 2025-04-17 Thu
    End: 2025-04-19 Sat
    File: test.ics
    |}]

let%expect_test "format_events aligns columns across events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Unix.putenv "TZ" "UTC";
  let events = [ parse_event ~fs timed_event_ics; parse_event ~fs all_day_event_ics ] in
  print_endline (Event.format_events ~format:`Text ~tz:utc events);
  [%expect {|
    work  2025-04-17 Thu 13:00 - 14:00 (Europe/London)  Team meeting @Room 3  timed-event
    work  2025-04-17 Thu - 2025-04-19 Sat               Conference            all-day-event
    |}]
