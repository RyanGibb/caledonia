open Cmdliner
open Caledonia_lib
open Query_args

let format_alarm_fire ?tz ?get_color (af : Component.alarm_fire) =
  let fire_time = af.fire_time in
  let summary = match Component.get_summary af.component with
    | Some s -> s
    | None -> "(no summary)"
  in
  let calendar = Component.get_calendar_name af.component in
  let trigger_str = match Format_utils.alarm_trigger af.alarm with
    | Some (_, `Duration span) -> Format_utils.format_alarm_trigger span
    | Some (_, `Datetime _) -> "at fixed time"
    | None -> ""
  in
  let fire_str = Format_utils.format_date ?tz fire_time in
  let fire_time_str =
    let dt = Date.ptime_to_timedesc ?tz fire_time in
    Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
  in
  let color = match get_color with Some f -> f calendar | None -> None in
  let cal_str = match color with
    | Some c -> Format_utils.colorize ~color:c calendar
    | None -> calendar
  in
  Printf.sprintf "%s %s  %s  %s  %s" fire_str fire_time_str trigger_str summary cal_str

let format_alarm_fires_text ?tz ?get_color fires =
  if fires = [] then "No alarms in range."
  else
    let data = List.map (fun (af : Component.alarm_fire) ->
      let fire_date = Format_utils.format_date ?tz af.fire_time in
      let fire_time =
        let dt = Date.ptime_to_timedesc ?tz af.fire_time in
        Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
      in
      let trigger_str = match Format_utils.alarm_trigger af.alarm with
        | Some (_, `Duration span) -> Format_utils.format_alarm_trigger span
        | Some (_, `Datetime _) -> "at fixed time"
        | None -> ""
      in
      let summary = match Component.get_summary af.component with
        | Some s -> s
        | None -> "(no summary)"
      in
      let calendar = Component.get_calendar_name af.component in
      (fire_date, fire_time, trigger_str, summary, calendar)
    ) fires in
    let max_date = Format_utils.max_width (fun (d, _, _, _, _) -> d) data in
    let max_time = Format_utils.max_width (fun (_, t, _, _, _) -> t) data in
    let max_trigger = Format_utils.max_width (fun (_, _, tr, _, _) -> tr) data in
    let max_summary = Format_utils.max_width (fun (_, _, _, s, _) -> s) data in
    let max_cal = Format_utils.max_width (fun (_, _, _, _, c) -> c) data in
    List.map (fun (fire_date, fire_time, trigger_str, summary, calendar) ->
      let color = match get_color with Some f -> f calendar | None -> None in
      Printf.sprintf "%s %s  %s  %s  %s"
        (Format_utils.pad_to_width max_date fire_date)
        (Format_utils.pad_to_width max_time fire_time)
        (Format_utils.pad_to_width max_trigger trigger_str)
        (Format_utils.pad_to_width max_summary summary)
        (Format_utils.pad_to_width ?color max_cal calendar)
    ) data
    |> String.concat "\n"

let format_alarm_fires_json ?tz fires =
  let json_fires = List.map (fun (af : Component.alarm_fire) ->
    let trigger_str = match Format_utils.alarm_trigger af.alarm with
      | Some (_, `Duration span) -> Format_utils.format_alarm_trigger span
      | Some (_, `Datetime _) -> "at fixed time"
      | None -> ""
    in
    `Assoc [
      ("fire_time", `String (Format_utils.format_date ?tz af.fire_time));
      ("trigger", `String trigger_str);
      ("summary", match Component.get_summary af.component with
        | Some s -> `String s | None -> `Null);
      ("calendar", `String (Component.get_calendar_name af.component));
      ("component_id", `String (Component.get_id af.component));
    ]
  ) fires in
  Yojson.Safe.to_string (`List json_fires)

let format_alarm_fires_entries ?tz fires =
  List.map (fun (af : Component.alarm_fire) ->
    let trigger_str = match Format_utils.alarm_trigger af.alarm with
      | Some (_, `Duration span) -> Format_utils.format_alarm_trigger span
      | Some (_, `Datetime _) -> "at fixed time"
      | None -> ""
    in
    let summary = match Component.get_summary af.component with
      | Some s -> s | None -> "(no summary)" in
    let fire_str = match tz with
      | Some tz -> Format_utils.format_date ~tz af.fire_time
      | None -> Format_utils.format_date af.fire_time
    in
    let fire_time_str =
      let dt = Date.ptime_to_timedesc ?tz af.fire_time in
      Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
    in
    Printf.sprintf "Summary: %s\nFire Time: %s %s\nTrigger: %s\nCalendar: %s\nID: %s\n"
      summary fire_str fire_time_str trigger_str
      (Component.get_calendar_name af.component)
      (Component.get_id af.component)
  ) fires
  |> String.concat "\n"

let run ?from_str ?to_str ~calendar:calendars ~format ~today ~tomorrow
    ~week ~month ?timezone ~fs calendar_dir =
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
  let components =
    match calendars with
    | [] -> components
    | cals -> List.filter (Component.in_calendars cals) components
  in
  let fires = Component.query_alarm_fires ~from ~to_ components in
  (if fires = [] then print_endline "No alarms in range."
   else
     let get_color cal_display_name =
       match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir cal_display_name with
       | Some cal_dir_name -> Calendar_dir.get_color ~fs calendar_dir cal_dir_name
       | None -> None
     in
     let output = match format with
       | `Text -> format_alarm_fires_text ~tz ~get_color fires
       | `Json -> format_alarm_fires_json ~tz fires
       | `Entries -> format_alarm_fires_entries ~tz fires
       | _ -> format_alarm_fires_text ~tz ~get_color fires
     in
     print_endline output);
  Ok ()

let format_arg =
  let doc = "Output format (text, json, entries)" in
  Arg.(
    value
    & opt (enum [("text", `Text); ("json", `Json); ("entries", `Entries)]) `Text
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let cmd ~fs calendar_dir =
  let run from_str to_str calendars format today tomorrow week month
      timezone () =
    match
      run ?from_str ?to_str ~calendar:calendars ~format ~today ~tomorrow
        ~week ~month ?timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ format_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ timezone_arg)
  in
  let doc = "List alarm fire times within a date range" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List when alarms fire within a specified date range. Shows the fire \
         time, trigger offset, event summary, and calendar for each alarm.";
      `P
        "By default, shows alarms firing from today to one month from today.";
      `S Manpage.s_examples;
      `I ("List alarms firing today:", "caled alarms --today");
      `I ("List alarms for the week:", "caled alarms --week");
      `I
        ( "List alarms in a date range:",
          "caled alarms --from 2025-04-01 --to 2025-04-30" );
      `I ("List alarms in JSON format:", "caled alarms --today --format json");
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "alarms" ~doc ~man ~exits:exit_info in
  Cmd.v info term
