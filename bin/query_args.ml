open Cmdliner
open Caledonia_lib

let from_arg =
  let doc =
    "Start date in YYYY-MM-DD format or a relative expression (today, \
     tomorrow, this-week, next-week, this-month, next-month, +Nd, -Nd, +Nw, \
     +Nm)"
  in
  let i = Arg.info [ "from"; "f" ] ~docv:"DATE" ~doc in
  Arg.(value @@ opt (some string) None i)

let to_arg =
  let doc =
    "End date in YYYY-MM-DD format or a relative expression (today, tomorrow, \
     this-week, next-week, this-month, next-month, +Nd, -Nd, +Nw, +Nm)"
  in
  let i = Arg.info [ "to"; "t" ] ~docv:"DATE" ~doc in
  Arg.(value @@ opt (some string) None i)

let calendar_arg =
  let doc = "Calendar to filter by" in
  Arg.(
    value
    & opt (some string) None
    & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let format_enum =
  [
    ("text", `Text);
    ("entries", `Entries);
    ("json", `Json);
    ("csv", `Csv);
    ("ics", `Ics);
    ("sexp", `Sexp);
  ]

let format_arg =
  let doc = "Output format (text, entries, json, csv, ics, sexp)" in
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

let timezone_arg =
  let doc =
    "Timezone to use for date calculations (e.g., 'America/New_York', 'UTC', \
     'Europe/London') defaulting to the system timezone"
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "timezone"; "z" ] ~docv:"TIMEZONE" ~doc)

let sort_field_enum =
  [
    ("start", `Start);
    ("end", `End);
    ("summary", `Summary);
    ("location", `Location);
    ("calendar", `Calendar);
  ]

type sort_spec = {
  field : [ `Start | `End | `Summary | `Location | `Calendar ];
  descending : bool;
}

let parse_sort_spec str =
  let ( let* ) = Result.bind in
  let parts = String.split_on_char ':' str in
  match parts with
  | [] -> Error (`Msg "Empty sort specification")
  | field_str :: order_opt -> (
      let* descending =
        match order_opt with
        | [ "desc" ] | [ "descending" ] -> Ok true
        | [ "asc" ] | [ "ascending" ] -> Ok false
        | [] -> Ok false (* Default to ascending *)
        | _ -> Error (`Msg ("Invalid sort order in: " ^ str))
      in
      match List.assoc_opt field_str sort_field_enum with
      | Some field -> Ok { field; descending }
      | None ->
          Error
            (`Msg
               (Printf.sprintf "Invalid sort field '%s'. Valid options are: %s"
                  field_str
                  (String.concat ", " (List.map fst sort_field_enum)))))

let sort_converter =
  let parse s = parse_sort_spec s in
  let print ppf spec =
    let field_str =
      List.find_map
        (fun (name, field) -> if field = spec.field then Some name else None)
        sort_field_enum
    in
    let order_str = if spec.descending then ":desc" else "" in
    Fmt.pf ppf "%s%s" (Option.value field_str ~default:"unknown") order_str
  in
  Arg.conv (parse, print)

let default_sort = { field = `Start; descending = false }

let sort_arg =
  let doc =
    "Sorting specifications in the format 'field[:order]' where field is one \
     of 'start', 'end', 'summary', 'location', 'calendar' and order is one of \
     'asc'/'ascending' or 'desc'/'descending' (default: asc). Multiple sort \
     specs can be provided for multi-level sorting. When no sort is specified, \
     defaults to sorting by start time ascending."
  in
  Arg.(
    value
    & opt_all sort_converter [ default_sort ]
    & info [ "sort"; "s" ] ~docv:"SORT" ~doc)

(* Convert sort specs to an instance comparator *)
let create_instance_comparator sort_specs =
  match sort_specs with
  | [] -> Recur.Instance.by_start
  | [ spec ] ->
      let comp =
        match spec.field with
        | `Start -> Recur.Instance.by_start
        | `End -> Recur.Instance.by_end
        | `Summary -> Recur.Instance.by_event Event.by_summary
        | `Location -> Recur.Instance.by_event Event.by_location
        | `Calendar -> Recur.Instance.by_event Event.by_collection
      in
      if spec.descending then Recur.Instance.descending comp else comp
  | specs ->
      (* Chain multiple sort specs together *)
      List.fold_right
        (fun spec acc ->
          let comp =
            match spec.field with
            | `Start -> Recur.Instance.by_start
            | `End -> Recur.Instance.by_end
            | `Summary -> Recur.Instance.by_event Event.by_summary
            | `Location -> Recur.Instance.by_event Event.by_location
            | `Calendar -> Recur.Instance.by_event Event.by_collection
          in
          let comp =
            if spec.descending then Recur.Instance.descending comp else comp
          in
          Recur.Instance.chain comp acc)
        (List.tl specs)
        (let spec = List.hd specs in
         let comp =
           match spec.field with
           | `Start -> Recur.Instance.by_start
           | `End -> Recur.Instance.by_end
           | `Summary -> Recur.Instance.by_event Event.by_summary
           | `Location -> Recur.Instance.by_event Event.by_location
           | `Calendar -> Recur.Instance.by_event Event.by_collection
         in
         if spec.descending then Recur.Instance.descending comp else comp)

let date_format_manpage_entries =
  [
    `S "DATE FORMATS";
    `P "Date format flags:";
    `I ("--today, -d", "Show events for today only");
    `I ("--tomorrow", "Show events for tomorrow only");
    `I ("--week, -w", "Show events for the current week");
    `I ("--month, -m", "Show events for the current month");
    `I
      ( "--timezone, -z",
        "Timezone to use for date calculations (e.g., 'America/New_York', \
         'UTC')" );
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

let parse_timezone ~timezone =
  match timezone with
  | Some tzid -> (
      match Timedesc.Time_zone.make tzid with
      | Some tz -> tz
      | None -> failwith ("Invalid timezone: " ^ tzid))
  | None -> !Date.default_timezone ()
