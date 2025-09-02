open Caledonia_lib

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let setup_fixed_date () =
  (Date.get_today := fun ?tz:_ () -> fixed_date);
  fixed_date

let%expect_test "parse date expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date expr parameter in
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
    today (to): 2025-03-27
    tomorrow (from): 2025-03-28
    tomorrow (to): 2025-03-28
    yesterday (from): 2025-03-26
    yesterday (to): 2025-03-26 |}]

let%expect_test "parse week expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date expr parameter in
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
    next-week (to): 2025-04-05 |}]

let%expect_test "parse month expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date expr parameter in
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
    let result = Result.get_ok @@ Date.parse_date expr parameter in
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
    +2w (to): 2025-04-12
    +1m (from): 2025-03-31
    +1m (to): 2025-04-30 |}]

let%expect_test "parse absolute date expressions" =
  let _ = setup_fixed_date () in
  let test_expr expr parameter =
    let result = Result.get_ok @@ Date.parse_date expr parameter in
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
