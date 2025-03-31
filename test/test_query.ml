(* Test the Query module *)

open Caledonia_lib

(* Setup a fixed date for testing *)
let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let setup_fixed_date () =
  (Date.get_today := fun () -> fixed_date);
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
  match Query.query ~fs calendar_dir ~from ~to_ () with
  | Ok instances ->
      Alcotest.(check int) "Should find events" 792 (List.length instances);
      let test_event =
        List.find_opt
          (fun instance ->
            Option.get @@ Event.get_summary instance.Recur.event = "Test Event")
          instances
      in
      Alcotest.(check bool) "Should find Test Event" true (test_event <> None)
  | Error _ -> Alcotest.fail "Error querying events"

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
  match Query.query ~fs calendar_dir ~from ~to_ () with
  | Ok instances ->
      let recurring_instances =
        List.filter
          (fun instance ->
            Option.get @@ Event.get_summary instance.Recur.event = "Recurring Event")
          instances
      in
      Alcotest.(check bool)
        "Should find multiple recurring event instances" true
        (List.length recurring_instances > 1)
  | Error _ -> Alcotest.fail "Error querying events"

let test_text_search ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let filter = Query.summary_contains "Test" in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int)
        "Should find event with 'Test' in summary" 2 (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  let filter = Query.location_contains "Weekly" in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int)
        "Should find event with 'Weekly' in location" 10 (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  let filter =
    Query.and_filter
      [ Query.summary_contains "Test"; Query.description_contains "test" ]
  in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int)
        "Should find events matching combined and criteria" 2
        (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  let filter =
    Query.or_filter
      [ Query.summary_contains "Test"; Query.location_contains "Weekly" ]
  in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int)
        "Should find events matching combined or criteria" 12
        (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  ()

let test_calendar_filter ~fs () =
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let collection = Collection.Col "example" in
  let filter = Query.in_collections [ collection ] in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      let all_match_calendar =
        List.for_all
          (fun e ->
            match Event.get_collection e.Recur.event with
            | id -> id = collection)
          instances
      in
      Alcotest.(check bool)
        (Printf.sprintf "All events should be from calendar '%s'"
           (match collection with Col str -> str))
        true all_match_calendar;
      Alcotest.(check int) "Should find events" 2 (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  let collections = [ Collection.Col "example"; Collection.Col "recurrence" ] in
  let filter = Query.in_collections collections in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int) "Should find events" 792 (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  let filter =
    Query.in_collections [ Collection.Col "non-existent-calendar" ]
  in
  (match Query.query ~fs calendar_dir ~from ~to_ ~filter () with
  | Ok instances ->
      Alcotest.(check int)
        "Should find 0 events for non-existent calendar" 0
        (List.length instances)
  | Error _ -> Alcotest.fail "Error querying events");
  ()

let test_events =
  (* Create a test event with specific text in all fields *)
  let create_test_event ~collection ~summary ~description ~location ~start =
    Event.create ~summary ~start
      ?description:(if description = "" then None else Some description)
      ?location:(if location = "" then None else Some location)
(Collection.Col collection)
  in
  [
    (* Event with text in all fields *)
    create_test_event ~collection:"search_test" ~summary:"Project Meeting"
      ~description:"Weekly project status meeting with team"
      ~location:"Conference Room A" ~start:fixed_date;
    (* Event with mixed case to test case insensitivity *)
    create_test_event ~collection:"search_test" ~summary:"IMPORTANT Meeting"
      ~description:"Critical project review with stakeholders"
      ~location:"Executive Suite" ~start:fixed_date;
    (* Event with word fragments *)
    create_test_event ~collection:"search_test" ~summary:"Conference Call"
      ~description:"International conference preparation"
      ~location:"Remote Meeting Room" ~start:fixed_date;
    (* Event with unique text in each field *)
    create_test_event ~collection:"search_test" ~summary:"Workshop on Testing"
      ~description:"Quality Assurance techniques and practices"
      ~location:"Training Center" ~start:fixed_date;
  ]

(* Test helper to verify if a list of events contains an event with a given summary *)
let contains_summary events summary =
  List.exists (fun e -> String.equal (Option.get @@ Event.get_summary e) summary) events

let test_case_insensitive_search () =
  (* Test lowercase query for an uppercase word *)
  let lowercase_filter = Query.summary_contains "important" in
  let matches =
    List.filter (fun e -> Query.matches_filter e lowercase_filter) test_events
  in
  Alcotest.(check bool)
    "Lowercase query should match uppercase text in summary" true
    (contains_summary matches "IMPORTANT Meeting");
  (* Test uppercase query for a lowercase word *)
  let uppercase_filter = Query.description_contains "WEEKLY" in
  let matches =
    List.filter (fun e -> Query.matches_filter e uppercase_filter) test_events
  in
  Alcotest.(check bool)
    "Uppercase query should match lowercase text in description" true
    (contains_summary matches "Project Meeting")

let test_partial_word_matching () =
  (* Test searching for part of a word *)
  let partial_filter = Query.summary_contains "Conf" in
  (* Should match "Conference" *)
  let matches =
    List.filter (fun e -> Query.matches_filter e partial_filter) test_events
  in
  Alcotest.(check bool)
    "Partial query should match full word in summary" true
    (contains_summary matches "Conference Call");
  (* Test another partial word in description *)
  let partial_filter = Query.description_contains "nation" in
  (* Should match "International" *)
  let matches =
    List.filter (fun e -> Query.matches_filter e partial_filter) test_events
  in
  Alcotest.(check bool)
    "Partial query should match within word in description" true
    (contains_summary matches "Conference Call");

  Alcotest.(check bool)
    "Partial query should match within word in description" true
    (contains_summary matches "Conference Call")

let test_boolean_logic () =
  (* Test AND filter *)
  let and_filter =
    Query.and_filter
      [ Query.summary_contains "Meeting"; Query.description_contains "project" ]
  in
  let matches =
    List.filter (fun e -> Query.matches_filter e and_filter) test_events
  in
  Alcotest.(check int)
    "AND filter should match events with both terms" 2
    (* Two events have both "Meeting" in summary and "project" in description *)
    (List.length matches);
  (* Test OR filter *)
  let or_filter =
    Query.or_filter
      [ Query.summary_contains "Workshop"; Query.summary_contains "Conference" ]
  in
  let matches =
    List.filter (fun e -> Query.matches_filter e or_filter) test_events
  in
  Alcotest.(check int)
    "OR filter should match events with either term"
    2 (* One event has "Workshop", one has "Conference" *)
    (List.length matches);

  (* Test NOT filter *)
  let not_filter = Query.not_filter (Query.summary_contains "Meeting") in
  let matches =
    List.filter (fun e -> Query.matches_filter e not_filter) test_events
  in
  Alcotest.(check int)
    "NOT filter should match events without the term"
    2 (* Two events don't have "Meeting" in the summary *)
    (List.length matches);
  (* Test complex combination: (Meeting AND project) OR Workshop BUT NOT Conference *)
  let complex_filter =
    Query.and_filter
      [
        Query.or_filter
          [
            Query.and_filter
              [
                Query.summary_contains "Meeting";
                Query.description_contains "project";
              ];
            Query.summary_contains "Workshop";
          ];
        Query.not_filter (Query.summary_contains "Conference");
      ]
  in
  let matches =
    List.filter (fun e -> Query.matches_filter e complex_filter) test_events
  in
  Alcotest.(check int)
    "Complex filter should match correctly"
    3 (* Three events should match the complex criteria *)
    (List.length matches)

let test_cross_field_search () =
  (* Search for a term that appears in multiple fields across different events *)
  let term_filter =
    Query.or_filter
      [
        Query.summary_contains "meeting";
        Query.description_contains "meeting";
        Query.location_contains "meeting";
      ]
  in
  let matches =
    List.filter (fun e -> Query.matches_filter e term_filter) test_events
  in
  Alcotest.(check int)
    "Cross-field search should find all occurrences"
    3 (* "meeting" appears in 3 events across different fields *)
    (List.length matches);
  (* Another test with a different term *)
  let term_filter =
    Query.or_filter
      [
        Query.summary_contains "conference";
        Query.description_contains "conference";
        Query.location_contains "conference";
      ]
  in
  let matches =
    List.filter (fun e -> Query.matches_filter e term_filter) test_events
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
    ("case insensitive search", `Quick, test_case_insensitive_search);
    ("partial word matching", `Quick, test_partial_word_matching);
    ("boolean logic filters", `Quick, test_boolean_logic);
    ("cross-field searching", `Quick, test_cross_field_search);
  ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  Alcotest.run "Query Tests" [ ("query", query_tests fs) ]
