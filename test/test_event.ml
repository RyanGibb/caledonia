(* Test the Query module *)

open Caledonia_lib

(* Setup a fixed date for testing *)
let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let setup_fixed_date () =
  (Date.get_today := fun ?tz:_ () -> fixed_date);
  fixed_date

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let test_query_all ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let events = Event.query events ~from ~to_ () in
  Alcotest.(check int) "Should find events" 791 (List.length events);
  let test_event =
    List.find_opt
      (fun event -> Option.get @@ Event.get_summary event = "Test Event")
      events
  in
  Alcotest.(check bool) "Should find Test Event" true (test_event <> None)

let test_recurrence_expansion ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 3, 1), ((0, 0, 0), 0)))
  in
  let to_ =
    Option.get @@ Ptime.of_date_time ((2025, 5, 31), ((23, 59, 59), 0))
  in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  let events = Event.query events ~from ~to_ () in
  let recurring_events =
    List.filter
      (fun event -> Option.get @@ Event.get_summary event = "Recurring Event")
      events
  in
  Alcotest.(check bool)
    "Should find multiple recurring event events" true
    (List.length recurring_events > 1)

let test_text_search ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let filter = Event.summary_contains "Test" in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int)
     "Should find event with 'Test' in summary" 2 (List.length events));
  let filter = Event.location_contains "Weekly" in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int)
     "Should find event with 'Weekly' in location" 10 (List.length events));
  let filter =
    Event.and_filter
      [ Event.summary_contains "Test"; Event.description_contains "test" ]
  in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int)
     "Should find events matching combined and criteria" 2 (List.length events));
  let filter =
    Event.or_filter
      [ Event.summary_contains "Test"; Event.location_contains "Weekly" ]
  in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int)
     "Should find events matching combined or criteria" 12 (List.length events));
  ()

let test_calendar_filter ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let calendar_name = "example" in
  let filter = Event.in_calendars [ calendar_name ] in
  let events = Result.get_ok @@ Calendar_dir.get_events ~fs calendar_dir in
  (let events = Event.query events ~from ~to_ ~filter () in
   let all_match_calendar =
     List.for_all
       (fun e ->
         match Event.get_calendar_name e with id -> id = calendar_name)
       events
   in
   Alcotest.(check bool)
     (Printf.sprintf "All events should be from calendar '%s'" calendar_name)
     true all_match_calendar;
   Alcotest.(check int) "Should find events" 2 (List.length events));
  let calendar_names = [ "example"; "recurrence" ] in
  let filter = Event.in_calendars calendar_names in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int) "Should find events" 791 (List.length events));
  let filter = Event.in_calendars [ "non-existent-calendar" ] in
  (let events = Event.query events ~from ~to_ ~filter () in
   Alcotest.(check int)
     "Should find 0 events for non-existent calendar" 0 (List.length events));
  ()

let test_events ~fs =
  (* Create a test event with specific text in all fields *)
  let create_test_event ~calendar_name ~summary ~description ~location ~start =
    Event.create ~fs ~calendar_dir_path ~summary ~start
      ?description:(if description = "" then None else Some description)
      ?location:(if location = "" then None else Some location)
      calendar_name
  in
  [
    (* Event with text in all fields *)
    Result.get_ok
    @@ create_test_event ~calendar_name:"search_test" ~summary:"Project Meeting"
         ~description:"Weekly project status meeting with team"
         ~location:"Conference Room A"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    (* Event with mixed case to test case insensitivity *)
    Result.get_ok
    @@ create_test_event ~calendar_name:"search_test"
         ~summary:"IMPORTANT Meeting"
         ~description:"Critical project review with stakeholders"
         ~location:"Executive Suite"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    (* Event with word fragments *)
    Result.get_ok
    @@ create_test_event ~calendar_name:"search_test" ~summary:"Conference Call"
         ~description:"International conference preparation"
         ~location:"Remote Meeting Room"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
    (* Event with unique text in each field *)
    Result.get_ok
    @@ create_test_event ~calendar_name:"search_test"
         ~summary:"Workshop on Testing"
         ~description:"Quality Assurance techniques and practices"
         ~location:"Training Center"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc fixed_date));
  ]

(* Test helper to verify if a list of events contains an event with a given summary *)
let contains_summary events summary =
  List.exists
    (fun e -> String.equal (Option.get @@ Event.get_summary e) summary)
    events

let test_case_insensitive_search ~fs () =
  (* Test lowercase query for an uppercase word *)
  let lowercase_filter = Event.summary_contains "important" in
  let matches =
    List.filter
      (fun e -> Event.matches_filter e lowercase_filter)
      (test_events ~fs)
  in
  Alcotest.(check bool)
    "Lowercase query should match uppercase text in summary" true
    (contains_summary matches "IMPORTANT Meeting");
  (* Test uppercase query for a lowercase word *)
  let uppercase_filter = Event.description_contains "WEEKLY" in
  let matches =
    List.filter
      (fun e -> Event.matches_filter e uppercase_filter)
      (test_events ~fs)
  in
  Alcotest.(check bool)
    "Uppercase query should match lowercase text in description" true
    (contains_summary matches "Project Meeting")

let test_partial_word_matching ~fs () =
  (* Test searching for part of a word *)
  let partial_filter = Event.summary_contains "Conf" in
  (* Should match "Conference" *)
  let matches =
    List.filter
      (fun e -> Event.matches_filter e partial_filter)
      (test_events ~fs)
  in
  Alcotest.(check bool)
    "Partial query should match full word in summary" true
    (contains_summary matches "Conference Call");
  (* Test another partial word in description *)
  let partial_filter = Event.description_contains "nation" in
  (* Should match "International" *)
  let matches =
    List.filter
      (fun e -> Event.matches_filter e partial_filter)
      (test_events ~fs)
  in
  Alcotest.(check bool)
    "Partial query should match within word in description" true
    (contains_summary matches "Conference Call");

  Alcotest.(check bool)
    "Partial query should match within word in description" true
    (contains_summary matches "Conference Call")

let test_boolean_logic ~fs () =
  (* Test AND filter *)
  let and_filter =
    Event.and_filter
      [ Event.summary_contains "Meeting"; Event.description_contains "project" ]
  in
  let matches =
    List.filter (fun e -> Event.matches_filter e and_filter) (test_events ~fs)
  in
  Alcotest.(check int)
    "AND filter should match events with both terms" 2
    (* Two events have both "Meeting" in summary and "project" in description *)
    (List.length matches);
  (* Test OR filter *)
  let or_filter =
    Event.or_filter
      [ Event.summary_contains "Workshop"; Event.summary_contains "Conference" ]
  in
  let matches =
    List.filter (fun e -> Event.matches_filter e or_filter) (test_events ~fs)
  in
  Alcotest.(check int)
    "OR filter should match events with either term"
    2 (* One event has "Workshop", one has "Conference" *)
    (List.length matches);

  (* Test NOT filter *)
  let not_filter = Event.not_filter (Event.summary_contains "Meeting") in
  let matches =
    List.filter (fun e -> Event.matches_filter e not_filter) (test_events ~fs)
  in
  Alcotest.(check int)
    "NOT filter should match events without the term"
    2 (* Two events don't have "Meeting" in the summary *)
    (List.length matches);
  (* Test complex combination: (Meeting AND project) OR Workshop BUT NOT Conference *)
  let complex_filter =
    Event.and_filter
      [
        Event.or_filter
          [
            Event.and_filter
              [
                Event.summary_contains "Meeting";
                Event.description_contains "project";
              ];
            Event.summary_contains "Workshop";
          ];
        Event.not_filter (Event.summary_contains "Conference");
      ]
  in
  let matches =
    List.filter
      (fun e -> Event.matches_filter e complex_filter)
      (test_events ~fs)
  in
  Alcotest.(check int)
    "Complex filter should match correctly"
    3 (* Three events should match the complex criteria *)
    (List.length matches)

let test_cross_field_search ~fs () =
  (* Search for a term that appears in multiple fields across different events *)
  let term_filter =
    Event.or_filter
      [
        Event.summary_contains "meeting";
        Event.description_contains "meeting";
        Event.location_contains "meeting";
      ]
  in
  let matches =
    List.filter (fun e -> Event.matches_filter e term_filter) (test_events ~fs)
  in
  Alcotest.(check int)
    "Cross-field search should find all occurrences"
    3 (* "meeting" appears in 3 events across different fields *)
    (List.length matches);
  (* Another test with a different term *)
  let term_filter =
    Event.or_filter
      [
        Event.summary_contains "conference";
        Event.description_contains "conference";
        Event.location_contains "conference";
      ]
  in
  let matches =
    List.filter (fun e -> Event.matches_filter e term_filter) (test_events ~fs)
  in
  Alcotest.(check int)
    "Cross-field search should find all occurrences of 'conference'"
    2 (* "conference" appears in 2 events across different fields *)
    (List.length matches)

let query_tests fs =
  [
    ("query all events", `Quick, test_query_all ~fs);
    ("recurrence expansion", `Quick, test_recurrence_expansion ~fs);
    ("text search", `Quick, test_text_search ~fs);
    ("calendar filter", `Quick, test_calendar_filter ~fs);
    ("case insensitive search", `Quick, test_case_insensitive_search ~fs);
    ("partial word matching", `Quick, test_partial_word_matching ~fs);
    ("boolean logic filters", `Quick, test_boolean_logic ~fs);
    ("cross-field searching", `Quick, test_cross_field_search ~fs);
  ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  Alcotest.run "Query Tests" [ ("query", query_tests fs) ]
