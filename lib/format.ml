type format = [ `Text | `TextId | `Json | `Csv | `Ics | `Entries | `Sexp ]

let format_date date =
  let y, m, d = Ptime.to_date date in
  let cal_date = CalendarLib.Date.make y m d in
  let weekday =
    match CalendarLib.Date.day_of_week cal_date with
    | CalendarLib.Date.Mon -> "Mon"
    | CalendarLib.Date.Tue -> "Tue"
    | CalendarLib.Date.Wed -> "Wed"
    | CalendarLib.Date.Thu -> "Thu"
    | CalendarLib.Date.Fri -> "Fri"
    | CalendarLib.Date.Sat -> "Sat"
    | CalendarLib.Date.Sun -> "Sun"
  in
  Printf.sprintf "%04d-%02d-%02d %s" y m d weekday

let format_time date =
  let _, ((h, m, _), _) = Ptime.to_date_time date in
  Printf.sprintf "%02d:%02d" h m

let format_datetime date =
  Printf.sprintf "%s %s" (format_date date) (format_time date)

let same_day d1 d2 =
  let y1, m1, d1 = Ptime.to_date d1 in
  let y2, m2, d2 = Ptime.to_date d2 in
  y1 == y2 && m1 == m2 && d1 == d2

(* exosed from icalendar *)

let weekday_strings =
  [
    (`Monday, "MO");
    (`Tuesday, "TU");
    (`Wednesday, "WE");
    (`Thursday, "TH");
    (`Friday, "FR");
    (`Saturday, "SA");
    (`Sunday, "SU");
  ]

let freq_strings =
  [
    (`Daily, "DAILY");
    (`Hourly, "HOURLY");
    (`Minutely, "MINUTELY");
    (`Monthly, "MONTHLY");
    (`Secondly, "SECONDLY");
    (`Weekly, "WEEKLY");
    (`Yearly, "YEARLY");
  ]

let date_to_str (y, m, d) = Printf.sprintf "%04d%02d%02d" y m d

let datetime_to_str ptime utc =
  let date, ((hh, mm, ss), _) = Ptime.to_date_time ptime in
  Printf.sprintf "%sT%02d%02d%02d%s" (date_to_str date) hh mm ss
    (if utc then "Z" else "")

let timestamp_to_ics ts buf =
  Buffer.add_string buf
  @@
  match ts with
  | `Utc ts -> datetime_to_str ts true
  | `Local ts -> datetime_to_str ts false
  | `With_tzid (ts, _str) -> (* TODO *) datetime_to_str ts false

let recurs_to_ics (freq, count_or_until, interval, l) buf =
  let write_rulepart key value =
    Buffer.add_string buf key;
    Buffer.add_char buf '=';
    Buffer.add_string buf value
  in
  let int_list l = String.concat "," @@ List.map string_of_int l in
  let recur_to_ics = function
    | `Byminute byminlist -> write_rulepart "BYMINUTE" (int_list byminlist)
    | `Byday bywdaylist ->
        let wday (weeknumber, weekday) =
          (if weeknumber = 0 then "" else string_of_int weeknumber)
          ^ List.assoc weekday weekday_strings
        in
        write_rulepart "BYDAY" (String.concat "," @@ List.map wday bywdaylist)
    | `Byhour byhrlist -> write_rulepart "BYHOUR" (int_list byhrlist)
    | `Bymonth bymolist -> write_rulepart "BYMONTH" (int_list bymolist)
    | `Bymonthday bymodaylist ->
        write_rulepart "BYMONTHDAY" (int_list bymodaylist)
    | `Bysecond byseclist -> write_rulepart "BYSECOND" (int_list byseclist)
    | `Bysetposday bysplist -> write_rulepart "BYSETPOS" (int_list bysplist)
    | `Byweek bywknolist -> write_rulepart "BYWEEKNO" (int_list bywknolist)
    | `Byyearday byyrdaylist ->
        write_rulepart "BYYEARDAY" (int_list byyrdaylist)
    | `Weekday weekday ->
        write_rulepart "WKST" (List.assoc weekday weekday_strings)
  in
  write_rulepart "FREQ" (List.assoc freq freq_strings);
  (match count_or_until with
  | None -> ()
  | Some x -> (
      Buffer.add_char buf ';';
      match x with
      | `Count c -> write_rulepart "COUNT" (string_of_int c)
      | `Until enddate ->
          (* TODO cleanup *)
          Buffer.add_string buf "UNTIL=";
          timestamp_to_ics enddate buf));
  (match interval with
  | None -> ()
  | Some i ->
      Buffer.add_char buf ';';
      write_rulepart "INTERVAL" (string_of_int i));
  List.iter
    (fun recur ->
      Buffer.add_char buf ';';
      recur_to_ics recur)
    l

let format_alt ~format ~start ~end_ event =
  let open Event in
  match format with
  | `Text ->
      let start_date = format_date start in
      let start_time =
        match get_day_event event with
        | true -> ""
        | false -> " " ^ format_time start
      in
      let end_date, end_time =
        match end_ with
        | None -> ("", "")
        | Some end_ -> (
            match (get_day_event event, same_day start end_) with
            | true, true -> ("", "")
            | true, _ -> (" - " ^ format_date end_, "")
            | false, true -> ("", " - " ^ format_time end_)
            | false, _ -> (" - " ^ format_date end_, " " ^ format_time end_))
      in
      let summary =
        match get_summary event with
        | Some summary when summary <> "" -> " " ^ summary
        | _ -> ""
      in
      let location =
        match get_location event with
        | Some loc when loc <> "" -> " @" ^ loc
        | _ -> ""
      in
      Printf.sprintf "%s%s%s%s%s%s" start_date start_time end_date end_time
        summary location
  | `TextId ->
      let id = get_id event in
      let start_date = " " ^ format_date start in
      let start_time =
        match get_day_event event with
        | true -> ""
        | false -> " " ^ format_time start
      in
      let end_date, end_time =
        match end_ with
        | None -> ("", "")
        | Some end_ -> (
            match (get_day_event event, same_day start end_) with
            | true, true -> ("", "")
            | true, _ -> (" - " ^ format_date end_, "")
            | false, true -> ("", " - " ^ format_time end_)
            | false, _ -> (" - " ^ format_date end_, " " ^ format_time end_))
      in
      let summary =
        match get_summary event with
        | Some summary when summary <> "" -> " " ^ summary
        | _ -> ""
      in
      let location =
        match get_location event with
        | Some loc when loc <> "" -> " @" ^ loc
        | _ -> ""
      in
      Printf.sprintf "%-50s%s%s%s%s%s%s" id start_date start_time end_date
        end_time summary location
  | `Json ->
      let open Yojson.Safe in
      let json =
        `Assoc
          [
            ("id", `String (get_id event));
            ( "summary",
              match get_summary event with
              | Some summary -> `String summary
              | None -> `Null );
            ("start", `String (format_datetime start));
            ( "end",
              match end_ with
              | Some e -> `String (format_datetime e)
              | None -> `Null );
            ( "location",
              match get_location event with
              | Some loc -> `String loc
              | None -> `Null );
            ( "description",
              match get_description event with
              | Some desc -> `String desc
              | None -> `Null );
            ( "calendar",
              match get_collection event with
              | Collection.Col cal -> `String cal );
          ]
      in
      to_string json
  | `Csv ->
      let summary =
        match get_summary event with Some summary -> summary | None -> ""
      in
      let start = format_datetime start in
      let end_str =
        match end_ with Some e -> format_datetime e | None -> ""
      in
      let location =
        match get_location event with Some loc -> loc | None -> ""
      in
      let cal_id =
        match get_collection event with Collection.Col cal -> cal
      in
      Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary start end_str
        location cal_id
  | `Ics ->
      let cal_props = [] in
      let event_ical = Event.to_icalendar event in
      Icalendar.to_ics ~cr:true (cal_props, [ `Event event_ical ])
  | `Entries ->
      let summary =
        match get_summary event with Some summary -> summary | None -> ""
      in
      let start = format_datetime start in
      let end_str =
        match end_ with Some e -> format_datetime e | None -> ""
      in
      let location =
        match get_location event with Some loc -> loc | None -> ""
      in
      let description =
        match get_description event with Some desc -> desc | None -> ""
      in
      let rrule =
        match get_recurrence event with
        | Some r ->
            let buf = Buffer.create 128 in
            recurs_to_ics r buf;
            Buffer.contents buf
        | None -> ""
      in
      Printf.sprintf "%s: %s\n%s: %s\n%s: %s\n%s: %s\n%s: %s\n%s: %s" "Summary"
        summary "Start" start "End" end_str "Location" location "Description"
        description "Reccurence" rrule
  | `Sexp ->
      let summary =
        match get_summary event with Some summary -> summary | None -> ""
      in
      let start_date, start_time =
        let date = start in
        let y, m, d = Ptime.to_date date in
        let _, ((h, min, s), _) = Ptime.to_date_time date in
        let cal_date = CalendarLib.Date.make y m d in
        let dow =
          match CalendarLib.Date.day_of_week cal_date with
          | CalendarLib.Date.Mon -> "monday"
          | CalendarLib.Date.Tue -> "tuesday"
          | CalendarLib.Date.Wed -> "wednesday"
          | CalendarLib.Date.Thu -> "thursday"
          | CalendarLib.Date.Fri -> "friday"
          | CalendarLib.Date.Sat -> "saturday"
          | CalendarLib.Date.Sun -> "sunday"
        in
        ( Printf.sprintf "(%04d %02d %02d %s)" y m d dow,
          Printf.sprintf "(%02d %02d %02d)" h min s )
      in
      let end_str =
        match end_ with
        | Some end_date ->
            let y, m, d = Ptime.to_date end_date in
            let _, ((h, min, s), _) = Ptime.to_date_time end_date in
            let cal_date = CalendarLib.Date.make y m d in
            let dow =
              match CalendarLib.Date.day_of_week cal_date with
              | CalendarLib.Date.Mon -> "monday"
              | CalendarLib.Date.Tue -> "tuesday"
              | CalendarLib.Date.Wed -> "wednesday"
              | CalendarLib.Date.Thu -> "thursday"
              | CalendarLib.Date.Fri -> "friday"
              | CalendarLib.Date.Sat -> "saturday"
              | CalendarLib.Date.Sun -> "sunday"
            in
            Printf.sprintf "((%04d %02d %02d %s) (%02d %02d %02d))" y m d dow h
              min s
        | None -> "nil"
      in
      let location =
        match get_location event with
        | Some loc -> Printf.sprintf "\"%s\"" (String.escaped loc)
        | None -> "nil"
      in
      let description =
        match get_description event with
        | Some desc -> Printf.sprintf "\"%s\"" (String.escaped desc)
        | None -> "nil"
      in
      let calendar =
        match get_collection event with
        | Collection.Col cal -> Printf.sprintf "\"%s\"" (String.escaped cal)
      in
      let id = get_id event in
      Printf.sprintf
        "((:id \"%s\" :summary \"%s\" :start (%s %s) :end %s :location %s \
         :description %s :calendar %s))"
        (String.escaped id) (String.escaped summary) start_date start_time
        end_str location description calendar

let format_event ?(format = `Text) event =
  format_alt ~format ~start:(Event.get_start event) ~end_:(Event.get_end event)
    event

let format_instance ?(format = `Text) instance =
  let open Recur in
  format_alt ~format ~start:instance.start ~end_:instance.end_ instance.event

let format_events ?(format = `Text) events =
  match format with
  | `Json ->
      let json_events =
        List.map
          (fun e -> Yojson.Safe.from_string (format_event ~format:`Json e))
          events
      in
      Yojson.Safe.to_string (`List json_events)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n" (List.map (format_event ~format:`Csv) events)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_event ~format:`Sexp e) events)
      ^ ")"
  | _ -> String.concat "\n" (List.map (fun e -> format_event ~format e) events)

let format_instances ?(format = `Text) instances =
  match format with
  | `Json ->
      let json_instances =
        List.map
          (fun e -> Yojson.Safe.from_string (format_instance ~format:`Json e))
          instances
      in
      Yojson.Safe.to_string (`List json_instances)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n" (List.map (format_instance ~format:`Csv) instances)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_instance ~format:`Sexp e) instances)
      ^ ")"
  | _ ->
      String.concat "\n"
        (List.map (fun e -> format_instance ~format e) instances)
