open Cmdliner
open Caledonia_lib

let from_arg =
  let doc =
    "Start date in YYYY-MM-DD format or a relative expression (today, \
     tomorrow, this-week, next-week, this-month, next-month, +Nd, -Nd, +Nw, \
     +Nm)"
  in
  let i = Arg.info [ "from"; "f" ] ~docv:"DATE" ~doc in
  let parse_date s = Date.parse_date s `From in
  Arg.(value @@ opt (some (Cmdliner.Arg.conv (parse_date, Ptime.pp))) None i)

let to_arg =
  let doc =
    "End date in YYYY-MM-DD format or a relative expression (today, tomorrow, \
     this-week, next-week, this-month, next-month, +Nd, -Nd, +Nw, +Nm)"
  in
  let i = Arg.info [ "to"; "t" ] ~docv:"DATE" ~doc in
  let parse_date s = Date.parse_date s `From in
  Arg.(value @@ opt (some (Cmdliner.Arg.conv (parse_date, Ptime.pp))) None i)

let calendar_arg =
  let doc = "Calendar to filter by" in
  Arg.(
    value
    & opt (some string) None
    & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let format_enum =
  [
    ("text", `Text);
    ("id", `TextId);
    ("json", `Json);
    ("csv", `Csv);
    ("ics", `Ics);
    ("records", `Entries);
    ("sexp", `Sexp);
  ]

let format_arg =
  let doc = "Output format (text, id, json, csv, ics, table, sexp)" in
  Arg.(
    value
    & opt (enum format_enum) `Text
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let count_arg =
  let doc = "Maximum number of events to display" in
  Arg.(value & opt (some int) None & info [ "count"; "n" ] ~docv:"COUNT" ~doc)

let today_arg =
  let doc = "Show events for today only" in
  Arg.(value & flag & info [ "today"; "d" ] ~doc)

let tomorrow_arg =
  let doc = "Show events for tomorrow only" in
  Arg.(value & flag & info [ "tomorrow" ] ~doc)

let week_arg =
  let doc = "Show events for the current week" in
  Arg.(value & flag & info [ "week"; "w" ] ~doc)

let month_arg =
  let doc = "Show events for the current month" in
  Arg.(value & flag & info [ "month"; "m" ] ~doc)

let date_format_manpage_entries =
  [
    `S "DATE FORMATS";
    `P "Date format flags:";
    `I ("--today, -d", "Show events for today only");
    `I ("--tomorrow", "Show events for tomorrow only");
    `I ("--week, -w", "Show events for the current week");
    `I ("--month, -m", "Show events for the current month");
    `P "Relative date formats for --from and --to:";
    `I ("today", "Current day");
    `I ("tomorrow", "Next day");
    `I ("yesterday", "Previous day");
    `I ("this-week", "Start of current week");
    `I ("next-week", "Start of next week");
    `I ("this-month", "Start of current month");
    `I ("next-month", "Start of next month");
    `I ("+Nd", "N days from today (e.g., +7d for a week from today)");
    `I ("-Nd", "N days before today (e.g., -7d for a week ago)");
    `I ("+Nw", "N weeks from today (e.g., +4w for 4 weeks from today)");
    `I ("+Nm", "N months from today (e.g., +2m for 2 months from today)");
  ]

let convert_relative_date ~today ~tomorrow ~week ~month =
  if today then Some "today"
  else if tomorrow then Some "tomorrow"
  else if week then Some "this-week"
  else if month then Some "this-month"
  else None
