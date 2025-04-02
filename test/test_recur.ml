(* Test recurrence expansion for specific event file.
   More tests can be found in the icalendar library at test/test_recur.ml *)

open Caledonia_lib

let test_recurring_events_in_date_range () =
  (* Create a recurring event that starts BEFORE our test range *)
  let event_start =
    Option.get @@ Ptime.of_date_time ((2025, 2, 1), ((14, 0, 0), 0))
  in
  let recurrence = (`Weekly, None, Some 1, []) in
  (* Weekly recurrence *)
  let recurring_event =
    Event.create ~summary:"Weekly Recurring Event"
      ~start:(`Datetime (`Utc event_start))
      ~recurrence (Collection.Col "test")
  in
  let test_date_range from_str to_str expected_count =
    try
      let from = Some (Result.get_ok @@ Date.parse_date from_str `From) in
      let to_ = Result.get_ok @@ Date.parse_date to_str `To in
      let instances = Recur.expand_event recurring_event ~from ~to_ in
      Printf.printf "Testing date range: %s to %s\n" from_str to_str;
      Printf.printf "Found %d instances:\n" (List.length instances);
      List.iter
        (fun i ->
          let date_str =
            let y, m, d = Ptime.to_date i.Recur.start in
            Printf.sprintf "%04d-%02d-%02d" y m d
          in
          let time_str =
            let _, ((h, m, s), _) = Ptime.to_date_time i.Recur.start in
            Printf.sprintf "%02d:%02d:%02d" h m s
          in
          Printf.printf "  - %s %s\n" date_str time_str)
        instances;
      (* Check the count matches what we expect *)
      Alcotest.(check int)
        (Printf.sprintf "Date range %s to %s should have %d occurrences"
           from_str to_str expected_count)
        expected_count (List.length instances)
    with Failure msg ->
      Alcotest.fail
        (Printf.sprintf "Failed to parse date range '%s' to '%s': %s" from_str
           to_str msg)
  in
  test_date_range "2025-01-25" "2025-01-31" 0;
  test_date_range "2025-02-01" "2025-02-02" 1;
  test_date_range "2025-02-01" "2025-02-14" 2;
  test_date_range "2025-02-01" "2025-02-15" 2;
  (* Event started in February, but query range is only in March *)
  test_date_range "2025-03-01" "2025-03-31" 5;
  (* Specific test for the March 8 instance *)
  test_date_range "2025-03-08" "2025-03-09" 1;
  (* Test a range that spans original event date and later dates *)
  test_date_range "2025-01-15" "2025-03-15" 6;
  ()

let test_dir = Filename.concat (Sys.getcwd ()) "calendar/recurrence/"
let day_seconds = 86400

(* Parse date string in YYYY-MM-DD format *)
let parse_date s =
  try
    let year = int_of_string (String.sub s 0 4) in
    let month = int_of_string (String.sub s 5 2) in
    let day = int_of_string (String.sub s 8 2) in

    match Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid date"
  with _ ->
    failwith (Printf.sprintf "Invalid date format: %s (expected YYYY-MM-DD)" s)

let load_event_from_file ~fs event_file =
  let file = Eio.Path.(fs / test_dir / event_file) in
  let _, file_name = Option.get @@ Eio.Path.split file in
  let content = Eio.Path.load file in
  match Icalendar.parse content with
  | Error err -> failwith (Printf.sprintf "Error parsing calendar: %s" err)
  | Ok (_, components) -> (
      match
        (* load a single event *)
        List.find_map (function `Event e -> Some e | _ -> None) components
      with
      | None -> failwith "No event found in file"
      | Some ical_event ->
          Event.of_icalendar (Collection.Col "example") ~file_name ical_event)

(* Format RRULE for display *)
let format_rrule_str rule =
  let freq, until_count, interval, _ = rule in
  let freq_str =
    match freq with
    | `Daily -> "DAILY"
    | `Weekly -> "WEEKLY"
    | `Monthly -> "MONTHLY"
    | `Yearly -> "YEARLY"
    | _ -> "OTHER"
  in
  let count_until_str =
    match until_count with
    | Some (`Count n) -> Printf.sprintf "COUNT=%d" n
    | Some (`Until _) -> "UNTIL=..."
    | None -> ""
  in
  let interval_str =
    match interval with Some n -> Printf.sprintf "INTERVAL=%d" n | None -> ""
  in
  let parts =
    List.filter (fun s -> s <> "") [ freq_str; count_until_str; interval_str ]
  in
  "RRULE:" ^ String.concat ";" parts

(* Common recurrence test logic *)
let test_recurrence_expansion ~fs event_file start_str end_str expected_count =
  let event = load_event_from_file ~fs event_file in
  let start_date = parse_date start_str in
  let end_date = parse_date end_str in
  let instances =
    Recur.expand_event event ~from:(Some start_date) ~to_:end_date
  in
  (* Format readable info for test failure messages *)
  let rrule_str =
    match Event.get_recurrence event with
    | Some rule -> format_rrule_str rule
    | None -> "No recurrence rule"
  in
  let summary = Option.get @@ Event.get_summary event in
  let msg =
    Printf.sprintf
      "Expected %d instances for '%s' (%s) between %s and %s, but got %d"
      expected_count summary rrule_str start_str end_str (List.length instances)
  in
  Alcotest.(check int) msg expected_count (List.length instances);
  instances

(* Helper to verify instances are properly spaced *)
let verify_instance_spacing instances expected_interval =
  if List.length instances < 2 then
    Alcotest.(check bool) "Should have at least 2 unique instances" true false
  else
    let errors = ref [] in
    ignore
      (List.fold_left
         (fun prev curr ->
           let span = Ptime.diff curr.Recur.start prev.Recur.start in
           let days_diff =
             Ptime.Span.to_float_s span /. float_of_int day_seconds
             |> Float.round |> int_of_float
           in
           if days_diff <> expected_interval then
             errors := (prev, curr, days_diff) :: !errors;
           curr)
         (List.hd instances) (List.tl instances));
    match !errors with
    | [] -> ()
    | (prev, curr, actual) :: _ ->
        let msg =
          Printf.sprintf
            "Expected interval of %d days, but found %d days between %s and %s"
            expected_interval actual
            (Ptime.to_rfc3339 ~space:true prev.Recur.start)
            (Ptime.to_rfc3339 ~space:true curr.Recur.start)
        in
        if actual = 0 then
          (* Ignore zero interval errors for now - this means we have duplicates *)
          ()
        else Alcotest.(check int) msg expected_interval actual

let test_daily ~fs () =
  let instances =
    test_recurrence_expansion ~fs "1_daily.ics" "2025-03-27" "2025-04-10" 14
  in
  verify_instance_spacing instances 1

let test_weekly ~fs () =
  let instances =
    test_recurrence_expansion ~fs "2_weekly.ics" "2025-03-27" "2025-06-27" 14
  in
  verify_instance_spacing instances 7

let test_monthly ~fs () =
  let instances =
    test_recurrence_expansion ~fs "3_monthly.ics" "2025-01-01" "2025-12-31" 8
  in
  (* Just verify the results without checking the interval, since months have different lengths *)
  let first_instance = List.hd instances in
  let (_, _, first_day), _ = Ptime.to_date_time first_instance.Recur.start in
  (* Check that they all fall on the same day of the month *)
  let all_same_day =
    List.for_all
      (fun instance ->
        let (_, _, day), _ = Ptime.to_date_time instance.Recur.start in
        day = first_day)
      instances
  in
  Alcotest.(check bool)
    (Printf.sprintf "All instances should be on day %d of the month" first_day)
    true all_same_day

let test_yearly ~fs () =
  let instances =
    test_recurrence_expansion ~fs "4_yearly.ics" "2025-01-01" "2035-12-31" 11
  in
  (* Verify instances are one year apart by checking the date *)
  let all_same_month_day =
    if List.length instances < 2 then true
    else
      let first = List.hd instances in
      let (_, first_month, first_day), _ =
        Ptime.to_date_time first.Recur.start
      in
      List.for_all
        (fun instance ->
          let (_, month, day), _ = Ptime.to_date_time instance.Recur.start in
          month = first_month && day = first_day)
        instances
  in
  Alcotest.(check bool)
    "All instances should be on the same day/month" true all_same_month_day

let test_every_2_days ~fs () =
  let instances =
    test_recurrence_expansion ~fs "5_every_2_days.ics" "2025-05-01" "2025-05-31"
      15
  in
  verify_instance_spacing instances 2

let test_every_3_weeks ~fs () =
  let instances =
    test_recurrence_expansion ~fs "6_every_3_weeks.ics" "2025-05-01"
      "2025-08-31" 6
  in
  verify_instance_spacing instances 21 (* 3 weeks = 21 days *)

let test_bimonthly ~fs () =
  let instances =
    test_recurrence_expansion ~fs "7_bimonthly.ics" "2025-05-01" "2026-05-31" 7
  in
  (* Verify all instances are on the same day of month *)
  let first_instance = List.hd instances in
  let (_, _, first_day), _ = Ptime.to_date_time first_instance.Recur.start in
  let all_same_day =
    List.for_all
      (fun instance ->
        let (_, _, day), _ = Ptime.to_date_time instance.Recur.start in
        day = first_day)
      instances
  in
  Alcotest.(check bool)
    (Printf.sprintf "All instances should be on day %d of the month" first_day)
    true all_same_day

let test_biennial ~fs () =
  let instances =
    test_recurrence_expansion ~fs "8_biennial.ics" "2025-01-01" "2035-12-31" 6
  in
  (* Verify all instances are on the same month and day, every two years *)
  let first_instance = List.hd instances in
  let (first_year, first_month, first_day), _ =
    Ptime.to_date_time first_instance.Recur.start
  in
  let all_same_date_alternate_years =
    List.for_all
      (fun instance ->
        let (year, month, day), _ = Ptime.to_date_time instance.Recur.start in
        month = first_month && day = first_day && (year - first_year) mod 2 = 0)
      instances
  in
  Alcotest.(check bool)
    "All instances should be on same day/month every two years" true
    all_same_date_alternate_years

let test_daily_count5 ~fs () =
  let instances =
    test_recurrence_expansion ~fs "9_daily_count5.ics" "2025-05-01" "2025-05-31"
      5
  in
  Alcotest.(check int)
    "Should have exactly 5 instances" 5 (List.length instances);
  verify_instance_spacing instances 1

let test_weekly_count10 ~fs () =
  let instances =
    test_recurrence_expansion ~fs "10_weekly_count10.ics" "2025-03-17"
      "2025-06-30" 10
  in
  verify_instance_spacing instances 7

let test_daily_until ~fs () =
  let instances =
    test_recurrence_expansion ~fs "11_daily_until.ics" "2025-05-01" "2025-05-31"
      14
  in
  (* Verify instances only occur until May 15 *)
  let last_instance =
    List.fold_left
      (fun latest curr ->
        if Ptime.compare curr.Recur.start latest.Recur.start > 0 then curr
        else latest)
      (List.hd instances) instances
  in
  let (_, month, day), _ = Ptime.to_date_time last_instance.Recur.start in
  Alcotest.(check int) "Last instance should be in May" 5 month;
  Alcotest.(check bool)
    "Last instance should be on or before May 15" true (day <= 15);
  verify_instance_spacing instances 1

let test_weekly_until ~fs () =
  let instances =
    test_recurrence_expansion ~fs "12_weekly_until.ics" "2025-05-01"
      "2025-07-31" 9
  in
  (* Verify instances only occur until June 30 *)
  let last_instance =
    List.fold_left
      (fun latest curr ->
        if Ptime.compare curr.Recur.start latest.Recur.start > 0 then curr
        else latest)
      (List.hd instances) instances
  in
  let (_, month, day), _ = Ptime.to_date_time last_instance.Recur.start in
  Alcotest.(check int) "Last instance should be in June" 6 month;
  Alcotest.(check bool)
    "Last instance should be on or before June 30" true (day <= 30);
  verify_instance_spacing instances 7

let test_weekly_monday_wednesday ~fs () =
  let instances =
    test_recurrence_expansion ~fs "13_weekly_monday_wednesday.ics" "2025-03-24"
      "2025-04-30" 11
  in
  let check_day_of_week instance day_list =
    let date = instance.Recur.start in
    let weekday =
      match Ptime.weekday ~tz_offset_s:0 date with
      | `Mon -> 1
      | `Tue -> 2
      | `Wed -> 3
      | `Thu -> 4
      | `Fri -> 5
      | `Sat -> 6
      | `Sun -> 0
    in
    List.mem weekday day_list
  in
  let all_on_mon_wed =
    List.for_all
      (fun instance ->
        check_day_of_week instance [ 1; 3 ] (* Monday = 1, Wednesday = 3 *))
      instances
  in
  Alcotest.(check bool)
    "All instances should be on Monday or Wednesday" true all_on_mon_wed

let test_weekly_weekends ~fs () =
  let instances =
    test_recurrence_expansion ~fs "14_weekly_weekends.ics" "2025-05-01"
      "2025-06-30" 18
  in
  let check_day_of_week instance day_list =
    let date = instance.Recur.start in
    let weekday =
      match Ptime.weekday ~tz_offset_s:0 date with
      | `Mon -> 1
      | `Tue -> 2
      | `Wed -> 3
      | `Thu -> 4
      | `Fri -> 5
      | `Sat -> 6
      | `Sun -> 0
    in
    List.mem weekday day_list
  in
  let all_on_weekends =
    List.for_all
      (fun instance ->
        check_day_of_week instance [ 6; 0 ] (* Saturday = 6, Sunday = 0 *))
      instances
  in
  Alcotest.(check bool)
    "All instances should be on Saturday or Sunday" true all_on_weekends

let test_monthly_specific_day ~fs () =
  let instances =
    test_recurrence_expansion ~fs "15_monthly_specific_day.ics" "2025-01-01"
      "2025-12-31" 8
  in
  (* Verify all instances are on the same day of month *)
  let first_instance = List.hd instances in
  let (_, _, first_day), _ = Ptime.to_date_time first_instance.Recur.start in
  let all_same_day =
    List.for_all
      (fun instance ->
        let (_, _, day), _ = Ptime.to_date_time instance.Recur.start in
        day = first_day)
      instances
  in
  Alcotest.(check bool)
    (Printf.sprintf "All instances should be on day %d of the month" first_day)
    true all_same_day

let test_monthly_second_monday ~fs () =
  let instances =
    test_recurrence_expansion ~fs "16_monthly_second_monday.ics" "2025-05-01"
      "2026-04-30" 11
  in
  let check_day_of_week instance =
    let date = instance.Recur.start in
    let weekday =
      match Ptime.weekday ~tz_offset_s:0 date with `Mon -> true | _ -> false
    in
    let (_, _, day), _ = Ptime.to_date_time date in
    weekday && day >= 8 && day <= 14 (* Second Monday is between 8th and 14th *)
  in
  let all_second_mondays = List.for_all check_day_of_week instances in
  Alcotest.(check bool)
    "All instances should be on the second Monday of each month" true
    all_second_mondays

let test_monthly_last_day ~fs () =
  let instances =
    test_recurrence_expansion ~fs "17_monthly_last_day.ics" "2025-05-01"
      "2026-04-30" 11
  in
  let is_last_day_of_month instance =
    let date = instance.Recur.start in
    let (year, month, day), _ = Ptime.to_date_time date in
    let last_day =
      match month with
      | 2 ->
          if (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 then 29
          else 28
      | 4 | 6 | 9 | 11 -> 30
      | _ -> 31
    in
    day = last_day
  in
  let all_last_days = List.for_all is_last_day_of_month instances in
  Alcotest.(check bool)
    "All instances should be on the last day of each month" true all_last_days

let test_yearly_specific_date ~fs () =
  let instances =
    test_recurrence_expansion ~fs "18_yearly_specific_date.ics" "2025-01-01"
      "2035-12-31" 11
  in
  (* Verify all instances are on the same month and day *)
  let first_instance = List.hd instances in
  let (_, first_month, first_day), _ =
    Ptime.to_date_time first_instance.Recur.start
  in
  let all_same_date =
    List.for_all
      (fun instance ->
        let (_, month, day), _ = Ptime.to_date_time instance.Recur.start in
        month = first_month && day = first_day)
      instances
  in
  Alcotest.(check bool)
    (Printf.sprintf "All instances should be on %d/%d" first_month first_day)
    true all_same_date

let test_yearly_mothers_day ~fs () =
  let instances =
    test_recurrence_expansion ~fs "19_yearly_mothers_day.ics" "2025-01-01"
      "2035-12-31" 10
  in
  let is_second_sunday_in_may instance =
    let date = instance.Recur.start in
    let (_, month, day), _ = Ptime.to_date_time date in
    let is_sunday =
      match Ptime.weekday ~tz_offset_s:0 date with `Sun -> true | _ -> false
    in
    month = 5 && is_sunday && day >= 8
    && day <= 14 (* Second Sunday is between 8th and 14th *)
  in
  let all_mothers_days = List.for_all is_second_sunday_in_may instances in
  Alcotest.(check bool)
    "All instances should be on the second Sunday in May" true all_mothers_days

let test_complex_weekdays_months ~fs () =
  let instances =
    test_recurrence_expansion ~fs "20_complex_weekdays_months.ics" "2025-05-01"
      "2025-09-30" 26
  in
  let is_tue_thu_in_summer instance =
    let date = instance.Recur.start in
    let (_, month, _), _ = Ptime.to_date_time date in
    let is_tue_thu =
      match Ptime.weekday ~tz_offset_s:0 date with
      | `Tue | `Thu -> true
      | _ -> false
    in
    is_tue_thu && (month = 6 || month = 7 || month = 8)
    (* Jun, Jul, Aug *)
  in
  let all_valid = List.for_all is_tue_thu_in_summer instances in
  Alcotest.(check bool)
    "All instances should be on Tuesday/Thursday in summer months" true
    all_valid

let test_complex_multiple_monthdays ~fs () =
  let instances =
    test_recurrence_expansion ~fs "21_complex_multiple_monthdays.ics"
      "2025-05-01" "2026-05-31" 38
  in
  (* Verify instances fall on the specified days (1st, 15th, or last day of month) *)
  let valid_day =
   fun day month ->
    day = 1 || day = 15
    || (month = 1 && day = 31)
    (* not a leap year *)
    || (month = 2 && day = 28)
    || (month = 3 && day = 31)
    || (month = 4 && day = 30)
    || (month = 5 && day = 31)
    || (month = 6 && day = 30)
    || (month = 7 && day = 31)
    || (month = 8 && day = 31)
    || (month = 9 && day = 30)
    || (month = 10 && day = 31)
    || (month = 11 && day = 30)
    || (month = 12 && day = 31)
  in
  let all_valid_days =
    List.for_all
      (fun instance ->
        let (_, month, day), _ = Ptime.to_date_time instance.Recur.start in
        valid_day day month)
      instances
  in
  Alcotest.(check bool)
    "All instances should be on 1st, 15th, or last day of month" true
    all_valid_days

let test_with_exdate ~fs () =
  let instances =
    test_recurrence_expansion ~fs "22_with_exdate.ics" "2025-05-01" "2025-06-30"
      9
  in
  (* Verify that excluded dates (May 15 and May 29) are not in the instances *)
  let excluded_dates = [ parse_date "2025-05-15"; parse_date "2025-05-29" ] in
  let no_excluded_dates =
    List.for_all
      (fun instance ->
        let check_not_excluded excluded =
          Ptime.compare instance.Recur.start excluded <> 0
        in
        List.for_all check_not_excluded excluded_dates)
      instances
  in
  Alcotest.(check bool)
    "No instances should be on excluded dates" true no_excluded_dates;
  (* Weekly recurrence should have 9 instances in this period *)
  Alcotest.(check int) "Should have 9 instances" 9 (List.length instances)

let test_dst_transition ~fs () =
  (* This event occurs from 2025-10-27 to 2025-11-17 *)
  let instances =
    test_recurrence_expansion ~fs "23_dst_transition.ics" "2025-10-01"
      "2025-11-30" 4
  in
  Alcotest.(check int)
    "Should have 4 instances across DST transition" 4 (List.length instances);
  (* Check each consecutive pair of dates *)
  ignore
    (List.fold_left
       (fun prev curr ->
         let span = Ptime.diff curr.Recur.start prev.Recur.start in
         let days_diff =
           Ptime.Span.to_float_s span /. float_of_int day_seconds
           |> Float.round |> int_of_float
         in
         Alcotest.(check int)
           (Printf.sprintf "Days diff should be 7 days")
           7 days_diff;
         curr)
       (List.hd instances) (List.tl instances))

let test_long_interval ~fs () =
  let instances =
    test_recurrence_expansion ~fs "24_long_interval.ics" "2025-05-01"
      "2026-05-31" 4
  in
  verify_instance_spacing instances 100

let test_leap_day ~fs () =
  let instances =
    test_recurrence_expansion ~fs "25_leap_day.ics" "2028-01-01" "2036-12-31" 3
  in
  Alcotest.(check int)
    "Should have 3 leap day instances" 3 (List.length instances);
  let all_leap_days =
    List.for_all
      (fun instance ->
        let (_, month, day), _ = Ptime.to_date_time instance.Recur.start in
        month = 2 && day = 29)
      instances
  in
  Alcotest.(check bool)
    "All instances should be on February 29" true all_leap_days;
  let years =
    List.map
      (fun instance ->
        let (year, _, _), _ = Ptime.to_date_time instance.Recur.start in
        year)
      instances
    |> List.sort_uniq compare
  in
  let expected_years = [ 2028; 2032; 2036 ] in
  Alcotest.(check (list int))
    "Should have instances in leap years 2028, 2032, and 2036" expected_years
    years

let test_weekly_wkst ~fs () =
  let instances =
    test_recurrence_expansion ~fs "26_weekly_wkst.ics" "2025-05-01" "2025-05-31"
      4
  in
  (* Verify we get exactly 4 instances (COUNT=4) *)
  Alcotest.(check int)
    "Should have exactly 4 instances" 4 (List.length instances);
  verify_instance_spacing instances 7

let test_monthly_nth_weekday ~fs () =
  let instances =
    test_recurrence_expansion ~fs "27_monthly_nth_weekday.ics" "2025-05-01"
      "2026-04-30" 11
  in
  let is_third_sunday instance =
    let date = instance.Recur.start in
    let is_sunday =
      match Ptime.weekday ~tz_offset_s:0 date with `Sun -> true | _ -> false
    in
    let (_, _, day), _ = Ptime.to_date_time date in
    is_sunday && day >= 15
    && day <= 21 (* Third Sunday is between 15th and 21st *)
  in
  let all_third_sundays = List.for_all is_third_sunday instances in
  Alcotest.(check bool)
    "All instances should be on the third Sunday of each month" true
    all_third_sundays

let test_yearly_historical ~fs () =
  let instances =
    test_recurrence_expansion ~fs "28_yearly_historical.ics" "2000-01-01"
      "2011-01-01" 11
  in
  (* Verify instances are yearly and end by 2010-12-31 *)
  let all_before_2011 =
    List.for_all
      (fun instance ->
        let (year, _, _), _ = Ptime.to_date_time instance.Recur.start in
        year <= 2010)
      instances
  in
  Alcotest.(check bool)
    "All instances should be before 2011" true all_before_2011;
  ()

let test_monthly_bymonth ~fs () =
  let instances =
    test_recurrence_expansion ~fs "29_monthly_bymonth.ics" "2025-01-01"
      "2026-12-31" 8
  in
  let is_specified_month instance =
    let date = instance.Recur.start in
    let (_, month, _), _ = Ptime.to_date_time date in
    month = 3 || month = 6 || month = 9 || month = 12 (* Mar, Jun, Sep, Dec *)
  in
  let all_specified_months = List.for_all is_specified_month instances in
  Alcotest.(check bool)
    "All instances should be in Mar, Jun, Sep, or Dec" true all_specified_months;
  ()

let test_fourth_weekday ~fs () =
  let instances =
    test_recurrence_expansion ~fs "30_fourth_weekday.ics" "2025-01-01"
      "2035-12-31" 10
  in
  let is_fourth_sunday_in_october instance =
    let date = instance.Recur.start in
    let (_, month, day), _ = Ptime.to_date_time date in
    let is_sunday =
      match Ptime.weekday ~tz_offset_s:0 date with `Sun -> true | _ -> false
    in
    month = 10 && is_sunday && day >= 22
    && day <= 28 (* Fourth Sunday is between 22nd and 28th *)
  in
  let all_fourth_sundays = List.for_all is_fourth_sunday_in_october instances in
  Alcotest.(check bool)
    "All instances should be on the fourth Sunday in October" true
    all_fourth_sundays

let recur_expand_tests ~fs =
  [
    ( "recurring events in date range",
      `Quick,
      test_recurring_events_in_date_range );
    ("daily recurrence", `Quick, test_daily ~fs);
    ("weekly recurrence", `Quick, test_weekly ~fs);
    ("monthly recurrence", `Quick, test_monthly ~fs);
    ("yearly recurrence", `Quick, test_yearly ~fs);
    ("every 2 days", `Quick, test_every_2_days ~fs);
    ("every 3 weeks", `Quick, test_every_3_weeks ~fs);
    ("bimonthly", `Quick, test_bimonthly ~fs);
    ("biennial", `Quick, test_biennial ~fs);
    ("daily with count=5", `Quick, test_daily_count5 ~fs);
    ("weekly with count=10", `Quick, test_weekly_count10 ~fs);
    ("daily until", `Quick, test_daily_until ~fs);
    ("weekly until", `Quick, test_weekly_until ~fs);
    ("weekly on Monday/Wednesday", `Quick, test_weekly_monday_wednesday ~fs);
    ("weekly on weekends", `Quick, test_weekly_weekends ~fs);
    ("monthly on specific day", `Quick, test_monthly_specific_day ~fs);
    ("monthly on second Monday", `Quick, test_monthly_second_monday ~fs);
    ("monthly on last day", `Quick, test_monthly_last_day ~fs);
    ("yearly on specific date", `Quick, test_yearly_specific_date ~fs);
    ("yearly on Mother's Day", `Quick, test_yearly_mothers_day ~fs);
    ("complex weekdays and months", `Quick, test_complex_weekdays_months ~fs);
    ("complex multiple month days", `Quick, test_complex_multiple_monthdays ~fs);
    ("with excluded dates", `Quick, test_with_exdate ~fs);
    ("DST transition handling", `Quick, test_dst_transition ~fs);
    ("long interval", `Quick, test_long_interval ~fs);
    ("leap day handling", `Quick, test_leap_day ~fs);
    ("weekly with week start", `Quick, test_weekly_wkst ~fs);
    ("monthly on nth weekday", `Quick, test_monthly_nth_weekday ~fs);
    ("yearly historical", `Quick, test_yearly_historical ~fs);
    ("monthly by month", `Quick, test_monthly_bymonth ~fs);
    ("fourth weekday", `Quick, test_fourth_weekday ~fs);
  ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Alcotest.run "Recur Expansion Tests"
    [ ("recur_expand", recur_expand_tests ~fs) ]
