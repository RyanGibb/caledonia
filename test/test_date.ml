open Caledonia_lib

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let london = Timedesc.Time_zone.make_exn "Europe/London"

let setup_fixed_date () =
  (* Pin the process TZ so functions that fall back to the local timezone
     are deterministic; the fixed "now" is passed explicitly at call sites. *)
  Unix.putenv "TZ" "Europe/London";
  fixed_date

let%expect_test "parse date expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date ~tz:london ~now:fixed_date expr parameter in
    let y, m, d = Ptime.to_date result in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  
  Printf.printf "today (from): %s\n" (test_expr "today" `From);
  Printf.printf "today (to): %s\n" (test_expr "today" `To);
  Printf.printf "tomorrow (from): %s\n" (test_expr "tomorrow" `From);
  Printf.printf "tomorrow (to): %s\n" (test_expr "tomorrow" `To);
  Printf.printf "yesterday (from): %s\n" (test_expr "yesterday" `From);
  Printf.printf "yesterday (to): %s\n" (test_expr "yesterday" `To);
  
  [%expect {|
    today (from): 2025-03-27
    today (to): 2025-03-28
    tomorrow (from): 2025-03-28
    tomorrow (to): 2025-03-29
    yesterday (from): 2025-03-26
    yesterday (to): 2025-03-27
    |}]

let%expect_test "parse week expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date ~tz:london ~now:fixed_date expr parameter in
    let y, m, d = Ptime.to_date result in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  
  Printf.printf "this-week (from): %s\n" (test_expr "this-week" `From);
  Printf.printf "this-week (to): %s\n" (test_expr "this-week" `To);
  Printf.printf "next-week (from): %s\n" (test_expr "next-week" `From);
  Printf.printf "next-week (to): %s\n" (test_expr "next-week" `To);
  
  [%expect {|
    this-week (from): 2025-03-24
    this-week (to): 2025-03-30
    next-week (from): 2025-03-30
    next-week (to): 2025-04-06
    |}]

let%expect_test "parse month expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date ~tz:london ~now:fixed_date expr parameter in
    let y, m, d = Ptime.to_date result in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  
  Printf.printf "this-month (from): %s\n" (test_expr "this-month" `From);
  Printf.printf "this-month (to): %s\n" (test_expr "this-month" `To);
  Printf.printf "next-month (from): %s\n" (test_expr "next-month" `From);
  Printf.printf "next-month (to): %s\n" (test_expr "next-month" `To);
  
  [%expect {|
    this-month (from): 2025-03-01
    this-month (to): 2025-03-31
    next-month (from): 2025-03-31
    next-month (to): 2025-04-30 |}]

let%expect_test "parse relative date expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date ~tz:london ~now:fixed_date expr parameter in
    let y, m, d = Ptime.to_date result in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  
  Printf.printf "+7d: %s\n" (test_expr "+7d" `From);
  Printf.printf "-7d: %s\n" (test_expr "-7d" `From);
  Printf.printf "+2w (from): %s\n" (test_expr "+2w" `From);
  Printf.printf "+2w (to): %s\n" (test_expr "+2w" `To);
  Printf.printf "+1m (from): %s\n" (test_expr "+1m" `From);
  Printf.printf "+1m (to): %s\n" (test_expr "+1m" `To);
  
  [%expect {|
    +7d: 2025-04-02
    -7d: 2025-03-20
    +2w (from): 2025-04-06
    +2w (to): 2025-04-13
    +1m (from): 2025-03-31
    +1m (to): 2025-04-30
    |}]

let%expect_test "parse absolute date expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date ~tz:london ~now:fixed_date expr parameter in
    let y, m, d = Ptime.to_date result in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  
  Printf.printf "2025-01-01: %s\n" (test_expr "2025-01-01" `From);
  Printf.printf "2025-01: %s\n" (test_expr "2025-01" `From);
  Printf.printf "2025: %s\n" (test_expr "2025" `From);
  Printf.printf "2025-3-1: %s\n" (test_expr "2025-3-1" `From);
  
  [%expect {|
    2025-01-01: 2025-01-01
    2025-01: 2025-01-01
    2025: 2025-01-01
    2025-3-1: 2025-03-01 |}]

let%expect_test "invalid date format" =
  let _ = setup_fixed_date () in
  let result = Date.parse_date "invalid-format" `From in
  (match result with
  | Error (`Msg msg) -> Printf.printf "Error (as expected): %s\n" (if String.length msg > 0 then "message received" else "empty message")
  | Ok _ -> Printf.printf "Unexpected success\n");
  [%expect {| Error (as expected): message received |}]

let%expect_test "timezone affects date calculations" =
  let utc = Timedesc.Time_zone.utc in
  let tokyo = Timedesc.Time_zone.make_exn "Asia/Tokyo" in (* UTC+9 *)
  let new_york = Timedesc.Time_zone.make_exn "America/New_York" in (* UTC-5 or UTC-4 *)

  (* Set a fixed UTC time: 2025-03-27 22:00:00 UTC *)
  (* This is 2025-03-28 07:00:00 in Tokyo (next day) *)
  (* This is 2025-03-27 18:00:00 in New York (same day) *)
  let fixed_utc_time = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((22, 0, 0), 0)) in

  (* Test that "today" is different in different timezones.
     We extract the local date via ptime_to_timedesc since Ptime.to_date
     always returns the UTC date. *)
  let today_utc = Date.parse_date ~tz:utc ~now:fixed_utc_time "today" `From in
  let today_tokyo = Date.parse_date ~tz:tokyo ~now:fixed_utc_time "today" `From in
  let today_ny = Date.parse_date ~tz:new_york ~now:fixed_utc_time "today" `From in

  let print_date name tz result =
    match result with
    | Ok ptime ->
        let dt = Date.ptime_to_timedesc ~tz ptime in
        Printf.printf "%s: %04d-%02d-%02d\n" name
          (Timedesc.year dt) (Timedesc.month dt) (Timedesc.day dt)
    | Error (`Msg msg) -> Printf.printf "%s: Error - %s\n" name msg
  in

  print_date "Today UTC" utc today_utc;
  print_date "Today Tokyo" tokyo today_tokyo;
  print_date "Today New York" new_york today_ny;

  [%expect {|
    Today UTC: 2025-03-27
    Today Tokyo: 2025-03-28
    Today New York: 2025-03-27 |}]

let%expect_test "get_start_of_week across timezone boundary" =
  (* Sunday 11pm UTC is Monday 8am in Tokyo *)
  let sunday_11pm_utc = Option.get @@ Ptime.of_date_time ((2025, 3, 30), ((23, 0, 0), 0)) in

  let old_tz = try Some (Unix.getenv "TZ") with Not_found -> None in

  (* Pin to UTC: it's Sunday March 30, so start of week is Monday March 24 *)
  Unix.putenv "TZ" "UTC";

  let start_of_week_utc = Date.get_start_of_week sunday_11pm_utc in
  let dt = Date.ptime_to_timedesc ~tz:Timedesc.Time_zone.utc start_of_week_utc in
  Printf.printf "Start of week (UTC): %04d-%02d-%02d\n"
    (Timedesc.year dt) (Timedesc.month dt) (Timedesc.day dt);

  (* Pin to Tokyo: it's already Monday March 31, so start of week is March 31 *)
  let tokyo = Timedesc.Time_zone.make_exn "Asia/Tokyo" in
  Unix.putenv "TZ" "Asia/Tokyo";

  let start_of_week_tokyo = Date.get_start_of_week sunday_11pm_utc in
  let dt = Date.ptime_to_timedesc ~tz:tokyo start_of_week_tokyo in
  Printf.printf "Start of week (Tokyo): %04d-%02d-%02d\n"
    (Timedesc.year dt) (Timedesc.month dt) (Timedesc.day dt);

  (* Restore *)
  (match old_tz with Some v -> Unix.putenv "TZ" v | None -> Unix.putenv "TZ" "Europe/London");

  [%expect {|
    Start of week (UTC): 2025-03-24
    Start of week (Tokyo): 2025-03-31 |}]

