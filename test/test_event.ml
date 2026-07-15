open Caledonia_lib

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let%expect_test "query all events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0))) in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let events = Event.query events ~from ~to_ () in
  Printf.printf "Number of events: %d\n" (List.length events);
  let test_event = List.find_opt (fun event -> Option.get @@ Event.get_summary event = "Test Event") events in
  Printf.printf "Found Test Event: %b\n" (test_event <> None);
  [%expect {|
    Number of events: 833
    Found Test Event: true
    |}]

let%expect_test "recurrence expansion" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (Option.get @@ Ptime.of_date_time ((2025, 3, 1), ((0, 0, 0), 0))) in
  let to_ = Option.get @@ Ptime.of_date_time ((2025, 5, 31), ((23, 59, 59), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let events = Event.query events ~from ~to_ () in
  let recurring_events = List.filter (fun event -> Option.get @@ Event.get_summary event = "Recurring Event") events in
  Printf.printf "Found multiple recurring events: %b\n" (List.length recurring_events > 1);
  Printf.printf "Number of recurring events: %d\n" (List.length recurring_events);
  [%expect {|
    Found multiple recurring events: true
    Number of recurring events: 10 |}]

let%expect_test "text search" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0))) in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  
  let filter = Event.summary_contains "Test" in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events with 'Test' in summary: %d\n" (List.length filtered);
  
  let filter = Event.location_contains "Weekly" in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events with 'Weekly' in location: %d\n" (List.length filtered);
  
  let filter = Event.and_filter [Event.summary_contains "Test"; Event.description_contains "test"] in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events matching AND criteria: %d\n" (List.length filtered);
  
  let filter = Event.or_filter [Event.summary_contains "Test"; Event.location_contains "Weekly"] in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events matching OR criteria: %d\n" (List.length filtered);
  
  [%expect {|
    Events with 'Test' in summary: 4
    Events with 'Weekly' in location: 10
    Events matching AND criteria: 3
    Events matching OR criteria: 14
    |}]

let%expect_test "calendar filter" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path in
  let from = Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0))) in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  
  let calendar_name = "example" in
  let filter = Event.in_calendars [calendar_name] in
  let filtered = Event.query events ~from ~to_ ~filter () in
  let all_match = List.for_all (fun e -> Event.get_calendar_name e = calendar_name) filtered in
  Printf.printf "All events from '%s': %b\n" calendar_name all_match;
  Printf.printf "Number of events: %d\n" (List.length filtered);
  
  let filter = Event.in_calendars ["example"; "recurrence"] in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events from multiple calendars: %d\n" (List.length filtered);
  
  let filter = Event.in_calendars ["non-existent-calendar"] in
  let filtered = Event.query events ~from ~to_ ~filter () in
  Printf.printf "Events from non-existent calendar: %d\n" (List.length filtered);
  
  [%expect {|
    All events from 'example': true
    Number of events: 3
    Events from multiple calendars: 792
    Events from non-existent calendar: 0
    |}]

let create_test_event ~fs ~calendar_name ~summary ~description ~location ~start =
  Event.create ~fs ~calendar_dir_path ~summary ~start
    ?description:(if description = "" then None else Some description)
    ?location:(if location = "" then None else Some location)
    calendar_name

let test_events ~fs =
  [
    Result.get_ok @@ create_test_event ~fs ~calendar_name:"search_test" 
      ~summary:"Project Meeting"
      ~description:"Weekly project status meeting with team"
      ~location:"Conference Room A"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    
    Result.get_ok @@ create_test_event ~fs ~calendar_name:"search_test"
      ~summary:"IMPORTANT Meeting"
      ~description:"Critical project review with stakeholders"
      ~location:"Executive Suite"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    
    Result.get_ok @@ create_test_event ~fs ~calendar_name:"search_test" 
      ~summary:"Conference Call"
      ~description:"International conference preparation"
      ~location:"Remote Meeting Room"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    
    Result.get_ok @@ create_test_event ~fs ~calendar_name:"search_test"
      ~summary:"Workshop on Testing"
      ~description:"Quality Assurance techniques and practices"
      ~location:"Training Center"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
  ]

let contains_summary events summary =
  List.exists (fun e -> String.equal (Option.get @@ Event.get_summary e) summary) events

let%expect_test "case insensitive search" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let events = test_events ~fs in
  
  let lowercase_filter = Event.summary_contains "important" in
  let matches = List.filter (fun e -> Event.matches_filter e lowercase_filter) events in
  Printf.printf "Lowercase 'important' matches 'IMPORTANT Meeting': %b\n" 
    (contains_summary matches "IMPORTANT Meeting");
  
  let uppercase_filter = Event.description_contains "WEEKLY" in
  let matches = List.filter (fun e -> Event.matches_filter e uppercase_filter) events in
  Printf.printf "Uppercase 'WEEKLY' matches 'Project Meeting': %b\n"
    (contains_summary matches "Project Meeting");
  
  [%expect {|
    Lowercase 'important' matches 'IMPORTANT Meeting': true
    Uppercase 'WEEKLY' matches 'Project Meeting': true |}]

let%expect_test "partial word matching" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let events = test_events ~fs in
  
  let partial_filter = Event.summary_contains "Conf" in
  let matches = List.filter (fun e -> Event.matches_filter e partial_filter) events in
  Printf.printf "Partial 'Conf' matches 'Conference Call': %b\n"
    (contains_summary matches "Conference Call");
  
  let partial_filter = Event.description_contains "nation" in
  let matches = List.filter (fun e -> Event.matches_filter e partial_filter) events in
  Printf.printf "Partial 'nation' matches 'Conference Call': %b\n"
    (contains_summary matches "Conference Call");
  
  [%expect {|
    Partial 'Conf' matches 'Conference Call': true
    Partial 'nation' matches 'Conference Call': true |}]

let%expect_test "boolean logic" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let events = test_events ~fs in
  
  let and_filter = Event.and_filter [Event.summary_contains "Meeting"; Event.description_contains "project"] in
  let matches = List.filter (fun e -> Event.matches_filter e and_filter) events in
  Printf.printf "AND filter (Meeting AND project): %d events\n" (List.length matches);
  
  let or_filter = Event.or_filter [Event.summary_contains "Workshop"; Event.summary_contains "Conference"] in
  let matches = List.filter (fun e -> Event.matches_filter e or_filter) events in
  Printf.printf "OR filter (Workshop OR Conference): %d events\n" (List.length matches);
  
  let not_filter = Event.not_filter (Event.summary_contains "Meeting") in
  let matches = List.filter (fun e -> Event.matches_filter e not_filter) events in
  Printf.printf "NOT filter (NOT Meeting): %d events\n" (List.length matches);
  
  let complex_filter = Event.and_filter [
    Event.or_filter [
      Event.and_filter [Event.summary_contains "Meeting"; Event.description_contains "project"];
      Event.summary_contains "Workshop"
    ];
    Event.not_filter (Event.summary_contains "Conference")
  ] in
  let matches = List.filter (fun e -> Event.matches_filter e complex_filter) events in
  Printf.printf "Complex filter: %d events\n" (List.length matches);
  
  [%expect {|
    AND filter (Meeting AND project): 2 events
    OR filter (Workshop OR Conference): 2 events
    NOT filter (NOT Meeting): 2 events
    Complex filter: 3 events |}]

let%expect_test "unknown TZID degrades to UTC instead of raising" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let ics =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:bad-tzid-event\r\n\
     DTSTAMP:20250101T000000Z\r\n\
     DTSTART;TZID=Mars/Olympus:20250315T100000\r\n\
     SUMMARY:Event with unknown timezone\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let calendar = Result.get_ok @@ Icalendar.parse ics in
  let file = Eio.Path.(fs / "dummy.ics") in
  let events = Event.events_of_icalendar "test" ~file calendar in
  let event = List.hd events in
  Printf.printf "start: %s\n" (Ptime.to_rfc3339 (Event.get_start event));
  [%expect {|
    Warning: unknown timezone Mars/Olympus, treating as UTC
    start: 2025-03-15T10:00:00-00:00
    |}]

let%expect_test "invalid duration yields no end time instead of raising" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let huge = Ptime.Span.of_int_s (86400 * 365 * 9000) in
  let event =
    Result.get_ok
    @@ Event.create ~fs ~calendar_dir_path ~summary:"Overflowing duration"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date))
         ~end_:(`Duration (Icalendar.Params.empty, huge))
         "example"
  in
  Printf.printf "end: %s\n"
    (match Event.get_end event with
    | Some t -> Ptime.to_rfc3339 t
    | None -> "none");
  [%expect {|
    Warning: invalid duration 283824000000.00s on event starting 2025-03-27T00:00:00-00:00, ignoring end time
    end: none
    |}]

let%expect_test "cross-field search" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let events = test_events ~fs in
  
  let term_filter = Event.or_filter [
    Event.summary_contains "meeting";
    Event.description_contains "meeting";
    Event.location_contains "meeting"
  ] in
  let matches = List.filter (fun e -> Event.matches_filter e term_filter) events in
  Printf.printf "Cross-field 'meeting': %d events\n" (List.length matches);
  
  let term_filter = Event.or_filter [
    Event.summary_contains "conference";
    Event.description_contains "conference";
    Event.location_contains "conference"
  ] in
  let matches = List.filter (fun e -> Event.matches_filter e term_filter) events in
  Printf.printf "Cross-field 'conference': %d events\n" (List.length matches);
  
  [%expect {|
    Cross-field 'meeting': 3 events
    Cross-field 'conference': 2 events |}]
