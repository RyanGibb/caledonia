open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar ?count ?query_text ~summary ~description
    ~categories ~id ~format ~today ~tomorrow ~week ~month ~component_type ?timezone ~sort ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filters = ref [] in
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
              let* d = Date.parse_date s `From in
              Ok (Some d)
        in
        let* to_ =
          match to_str with
          | None -> Ok None
          | Some s ->
              let* d = Date.parse_date s `To in
              Ok (Some d)
        in
        let max_date = Date.add_years (Date.get_today ()) 75 in
        match (from, to_) with
        | Some f, Some t -> Ok (Some f, Date.to_end_of_day t)
        | Some f, None -> Ok (Some f, Date.to_end_of_day max_date)
        | None, Some t -> Ok (None, Date.to_end_of_day t)
        | None, None -> Ok (None, Date.to_end_of_day max_date))
  in
  (match calendar with
  | [] -> ()
  | calendars -> filters := Component.in_calendars calendars :: !filters);
  (match query_text with
  | Some text ->
      (match (summary, description, categories) with
      | true, _, _ -> filters := Component.summary_contains text :: !filters
      | _, true, _ -> filters := Component.description_contains text :: !filters
      | _, _, true ->
          let cats = String.split_on_char ',' text |> List.map String.trim in
          filters := Component.has_categories cats :: !filters
      | false, false, false ->
          let cats = String.split_on_char ',' text |> List.map String.trim in
          filters :=
            Component.or_filter
              [
                Component.summary_contains text;
                Component.description_contains text;
                Component.has_categories cats;
              ]
            :: !filters)
  | None -> ());
  (match component_type with
  | "all" -> ()
  | "event" -> filters := Component.is_type CEvent :: !filters
  | "todo" -> filters := Component.is_type CTodo :: !filters
  | "journal" -> filters := Component.is_type CJournal :: !filters
  | _ -> ());
  (match id with
  | Some id -> filters := (fun c -> Component.get_id c = id) :: !filters
  | None -> ());
  let filter = Component.and_filter !filters in
  let comparator = Query_args.create_component_comparator sort in
  let* all_components = Calendar_dir.get_components ~fs calendar_dir in
  let components =
    let filtered = List.filter filter all_components in
    let all_todos = List.filter_map Component.to_todo all_components in
    let filtered_todos = List.filter_map Component.to_todo filtered in
    let expanded_todos = Todo.expand_with_ancestors ~all_todos ~filtered_todos in
    let expanded_todo_components = List.map Component.of_todo expanded_todos in
    let non_todo_filtered = List.filter (fun c -> match Component.component_type c with Component.CTodo -> false | _ -> true) filtered in
    let expanded_filtered = non_todo_filtered @ expanded_todo_components in
    let filtered_with_dates =
      List.filter (fun c ->
        match Component.get_start c with
        | None -> from = None && to_str = None
        | Some start ->
            let after_from = match from with
              | None -> true
              | Some f -> Ptime.compare start f >= 0
            in
            let before_to = Ptime.compare start to_ <= 0 in
            after_from && before_to) expanded_filtered
    in
    let sorted = List.sort comparator filtered_with_dates in
    match count with None -> sorted | Some n -> List.filteri (fun i _ -> i < n) sorted
  in
  (if components = [] then print_endline "No components found."
  else
    let get_color cal_display_name =
      match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir cal_display_name with
      | Some cal_dir_name -> Calendar_dir.get_color ~fs calendar_dir cal_dir_name
      | None -> None
    in
    print_endline (Component.format_components ~tz ~format ~get_color components));
  Ok ()

let query_text_arg =
  let doc =
    "Text to search for in the fields summary, description, and/or location."
  in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc)

let summary_arg =
  let doc = "Search in event summaries only" in
  Arg.(value & flag & info [ "summary"; "s" ] ~doc)

let description_arg =
  let doc = "Search in descriptions only" in
  Arg.(value & flag & info [ "description"; "D" ] ~doc)

let categories_arg =
  let doc = "Search in categories only" in
  Arg.(value & flag & info [ "categories" ] ~doc)

let component_type_filter_arg =
  let doc = "Filter by component type (all, event, todo, journal)" in
  let comp_type_enum = ["all"; "event"; "todo"; "journal"] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "all"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let id_arg =
  let doc = "Search for an event with a specific ID" in
  Arg.(value & opt (some string) None & info [ "id"; "i" ] ~docv:"ID" ~doc)

let cmd ~fs calendar_dir =
  let run query_text from_str to_str calendars count format summary description
      categories id today tomorrow week month component_type timezone
      sort () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ?query_text ~summary
        ~description ~categories ~id ~format ~today ~tomorrow ~week ~month
        ~component_type ?timezone ~sort ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ query_text_arg $ from_arg $ to_arg $ calendar_arg $ count_arg
      $ format_arg $ summary_arg $ description_arg $ categories_arg $ id_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ component_type_filter_arg
      $ timezone_arg $ sort_arg)
  in
  let doc = "Search calendar components for specific text" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Search calendar components for text in summary, description, or categories \
         fields. By default, the search looks across all text fields in all \
         components regardless of date. You can specify specific fields to search \
         in using the --summary, --description, or --categories flags. You can \
         use date flags to show components for a specific time period, and filter \
         components with the --sort option.";
      `S Manpage.s_examples;
      `I ("Search for 'meeting' in all components:", "caled search meeting");
      `I
        ( "Search for 'interview' in summaries only:",
          "caled search --summary interview" );
      `I
        ( "Search for 'work' in categories:",
          "caled search --categories work" );
      `I
        ( "Search for todos only:",
          "caled search --type todo" );
      `I
        ( "Search for 'project' in components this month:",
          "caled search --month project" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "search" ~doc ~man ~exits:exit_info in
  Cmd.v info term
