open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
    ~week ~month ?timezone ~sort ~component_type ~incomplete ~overdue ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let tz = Query_args.parse_timezone ~timezone in
  let* from, to_ =
    match
      Date.convert_relative_date_formats ~tz ~today ~tomorrow ~week ~month ()
    with
    | Some (from, to_) ->
        let* _ =
          match (from_str, to_str) with
          | None, None -> Ok ()
          | _ ->
              Error
                (`Msg
                   "Can't specify --from / --to when using --today, --week, \
                    --month")
        in
        Ok (Some from, to_)
    | None -> (
        let* from =
          match from_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date ~tz s `From in
              Ok (Some d)
        in
        let* to_ =
          match to_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date ~tz s `To in
              Ok (Some d)
        in
        match (from, to_) with
        | Some f, Some t -> Ok (Some f, Date.to_end_of_day t)
        | Some f, None ->
            let one_month_later = Date.add_months f 1 in
            Ok (Some f, one_month_later)
        | None, Some t -> Ok (None, Date.to_end_of_day t)
        | None, None ->
            let today_date = Date.get_today ~tz () in
            let one_month_later = Date.add_months today_date 1 in
            Ok (Some today_date, one_month_later))
  in
  let* components = Calendar_dir.get_components ~fs calendar_dir in

  let filters = [] in
  let filters =
    match component_type with
    | "all" -> filters
    | "event" -> Component.is_type CEvent :: filters
    | "todo" -> Component.is_type CTodo :: filters
    | "journal" -> Component.is_type CJournal :: filters
    | _ -> filters
  in
  let filters =
    match calendars with
    | [] -> filters
    | cals -> Component.in_calendars cals :: filters
  in
  let filters =
    if incomplete then
      (fun c -> match Component.to_todo c with
        | Some t -> not (Todo.is_completed t)
        | None -> false) :: filters
    else filters
  in
  let filters =
    if overdue then
      (fun c -> match Component.to_todo c with
        | Some t -> Todo.is_overdue t
        | None -> false) :: filters
    else filters
  in

  let filter = match filters with [] -> None | fs -> Some (Component.and_filter fs) in

  let comparator =
    match sort with
    | [] -> Component.by_start
    | sorts ->
        List.fold_left (fun acc s ->
          let open Query_args in
          let comp = match s.field with
          | `Start -> if s.descending then Component.descending Component.by_start else Component.by_start
          | `Summary -> if s.descending then Component.descending Component.by_summary else Component.by_summary
          | `Calendar -> if s.descending then Component.descending Component.by_calendar_name else Component.by_calendar_name
          | _ -> Component.by_start
          in
          Component.chain acc comp) Component.by_start sorts
  in

  let components =
    let filtered = match filter with
      | None -> components
      | Some f -> List.filter f components
    in
    let filtered =
      if component_type = "event" then
        filtered
        |> List.filter_map (fun c -> match Component.to_event c with
          | Some e ->
              let expanded = Event.expand_recurrences ~from ~to_ e in
              Some (List.map Component.of_event expanded)
          | None -> None)
        |> List.flatten
      else
        (* For "all" type, we need to expand recurring events but keep other components as-is *)
        filtered
        |> List.map (fun c ->
            match Component.to_event c with
            | Some e when Event.get_recurrence e <> None ->
                (* Expand recurring events *)
                let expanded = Event.expand_recurrences ~from ~to_ e in
                List.map Component.of_event expanded
            | _ ->
                (* For non-recurring events and other component types, filter by date range *)
                (match Component.get_start c with
                | None -> if from = None && to_str = None then [c] else []
                | Some start ->
                    let after_from = match from with
                      | None -> true
                      | Some f -> Ptime.compare start f >= 0
                    in
                    let before_to = Ptime.compare start to_ <= 0 in
                    if after_from && before_to then [c] else []))
        |> List.flatten
    in
    let sorted = List.sort comparator filtered in
    match count with
    | None -> sorted
    | Some n -> List.filteri (fun i _ -> i < n) sorted
  in

  (if components = [] then print_endline "No components found."
  else
    let get_color cal_display_name =
      match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir cal_display_name with
      | Some cal_dir_name -> Calendar_dir.get_color ~fs calendar_dir cal_dir_name
      | None -> None
    in
    print_endline (Component.format_components ~format ~tz ~get_color components));
  Ok ()

let incomplete_arg =
  let doc = "Show only incomplete todos" in
  Arg.(value & flag & info [ "incomplete" ] ~doc)

let overdue_arg =
  let doc = "Show only overdue todos" in
  Arg.(value & flag & info [ "overdue" ] ~doc)

let component_type_filter_arg =
  let doc = "Filter by component type (all, event, todo, journal)" in
  let comp_type_enum = ["all"; "event"; "todo"; "journal"] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "all"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let cmd ~fs calendar_dir =
  let run from_str to_str calendars count format today tomorrow week month
      timezone sort component_type incomplete overdue () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
        ~week ~month ?timezone ~sort ~component_type ~incomplete ~overdue ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ count_arg $ format_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ timezone_arg
      $ sort_arg $ component_type_filter_arg $ incomplete_arg $ overdue_arg)
  in
  let doc = "List calendar components (events, todos, journals)" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List calendar components within a specified date range. By default, \
         all components from today to one month from today are shown. You can use date \
         flags to show components for a specific time period, filter by type, \
         and sort with the --sort option.";
      `S Manpage.s_examples;
      `I ("List all events for today:", "caled list --today --type event");
      `I ("List all todos:", "caled list --type todo");
      `I ("List incomplete todos:", "caled list --type todo --incomplete");
      `I ("List overdue todos:", "caled list --type todo --overdue");
      `I ("List journal entries for the month:", "caled list --type journal --month");
      `I ("List all components for the current week:", "caled list --week");
      `I
        ( "List components within a specific date range:",
          "caled list --from 2025-03-27 --to 2025-04-01" );
      `I ("List components from a specific calendar:", "caled list --calendar work");
      `I ("List components in JSON format:", "caled list --format json");
      `I ("Limit the number of components shown:", "caled list --count 5");
      `I
        ( "Sort by component type then start time:",
          "caled list --sort type --sort start" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "list" ~doc ~man ~exits:exit_info in
  Cmd.v info term