(* Test the Date module *)

open Caledonia_lib

(* Setup a fixed date for testing *)
let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))

let setup_fixed_date () =
  (Date.get_today := fun ?tz:_ () -> fixed_date);
  fixed_date

let test_parse_date () =
  let test_expr expr parameter expected =
    try
      let result = Result.get_ok @@ Date.parse_date expr parameter in
      let result_str =
        let y, m, d = Ptime.to_date result in
        Printf.sprintf "%04d-%02d-%02d" y m d
      in
      Alcotest.(check string)
        (Printf.sprintf "'%s' %s should parse to '%s'" expr
           (match parameter with `From -> "from" | `To -> "to")
           expected)
        expected result_str
    with Failure msg ->
      Alcotest.fail (Printf.sprintf "Failed to parse '%s': %s" expr msg)
  in
  test_expr "today" `From "2025-03-27";
  test_expr "today" `To "2025-03-27";
  test_expr "tomorrow" `From "2025-03-28";
  test_expr "tomorrow" `To "2025-03-28";
  test_expr "yesterday" `From "2025-03-26";
  test_expr "yesterday" `To "2025-03-26";
  test_expr "this-week" `From "2025-03-24";
  test_expr "this-week" `To "2025-03-30";
  test_expr "next-week" `From "2025-03-31";
  test_expr "next-week" `To "2025-04-06";
  test_expr "this-month" `From "2025-03-01";
  test_expr "this-month" `To "2025-03-31";
  test_expr "next-month" `From "2025-04-01";
  test_expr "next-month" `To "2025-04-30";
  test_expr "+7d" `From "2025-04-03";
  test_expr "+7d" `To "2025-04-03";
  test_expr "-7d" `From "2025-03-20";
  test_expr "-7d" `To "2025-03-20";
  test_expr "+2w" `From "2025-04-07";
  test_expr "+2w" `To "2025-04-13";
  test_expr "+1m" `From "2025-04-01";
  test_expr "+1m" `To "2025-04-30";
  test_expr "2025-01-01" `From "2025-01-01";
  test_expr "2025-01-01" `To "2025-01-01";
  test_expr "2025-01" `From "2025-01-01";
  test_expr "2025-01" `To "2025-01-01";
  test_expr "2025" `From "2025-01-01";
  test_expr "2025" `To "2025-01-01";
  test_expr "2025-3-1" `From "2025-03-01";
  test_expr "2025-3-1" `To "2025-03-01";
  (try
     let _ = Result.get_ok @@ Date.parse_date "invalid-format" `From in
     Alcotest.fail "Should have raised an exception for invalid format"
   with Failure msg ->
     Alcotest.(check bool)
       "Invalid format should raise exception with appropriate message" true
       (String.length msg > 0));
  ()

let date_tests = [ ("date expression parsing", `Quick, test_parse_date) ]

let () =
  let _ = setup_fixed_date () in
  Alcotest.run "Query Tests" [ ("query", date_tests) ]
