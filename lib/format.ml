type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

let format_date ?tz date =
  let dt = Date.ptime_to_timedesc ?tz date in
  let y = Timedesc.year dt in
  let m = Timedesc.month dt in
  let d = Timedesc.day dt in
  let weekday =
    match Timedesc.weekday dt with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  Printf.sprintf "%04d-%02d-%02d %s" y m d weekday

let format_time ?tz date =
  let dt = Date.ptime_to_timedesc ?tz date in
  let h = Timedesc.hour dt in
  let m = Timedesc.minute dt in
  Printf.sprintf "%02d:%02d" h m

let format_datetime ?tz date =
  let tz_str =
    match tz with
    | Some tz -> Printf.sprintf "(%s)" (Timedesc.Time_zone.name tz)
    | None -> ""
  in
  Printf.sprintf "%s %s%s" (format_date ?tz date) (format_time ?tz date) tz_str

let next_day day ~next =
  let y1, m1, d1 = Ptime.to_date day in
  let y2, m2, d2 = Ptime.to_date next in
  y1 == y2 && m1 == m2 && d1 == d2 - 1

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

let format_alt ~format ~start ~end_ ?tz event =
  let open Event in
  match format with
  | `Text ->
      let id = get_id event in
      let start_date = " " ^ format_date ?tz start in
      let start_time =
        match is_date event with
        | true -> ""
        | false -> " " ^ format_time ?tz start
      in
      let end_date, end_time =
        match end_ with
        | None -> ("", "")
        | Some end_ -> (
            match (is_date event, next_day start ~next:end_) with
            | true, true -> ("", "")
            | true, _ -> (" - " ^ format_date ?tz end_, "")
            | false, true -> ("", " - " ^ format_time ?tz end_)
            | false, _ ->
                (" - " ^ format_date ?tz end_, " " ^ format_time ?tz end_))
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
      Printf.sprintf "%-45s%s%s%s%s%s%s" id start_date start_time end_date
        end_time summary location
  | `Entries ->
      let format_opt label f opt =
        Option.map (fun x -> Printf.sprintf "%s: %s\n" label (f x)) opt
        |> Option.value ~default:""
      in
      let format timezone datetime =
        match is_date event with
        | true -> format_date ?tz datetime
        | false -> (
            format_datetime ?tz datetime
            ^ match timezone with None -> "" | Some t -> " (" ^ t ^ ")")
      in
      let start_str =
        format_opt "Start" (format (get_start_timezone event)) (Some start)
      in
      let end_str = format_opt "End" (format (get_end_timezone event)) end_ in
      let location_str = format_opt "Location" Fun.id (get_location event) in
      let description_str =
        format_opt "Description" Fun.id (get_description event)
      in
      let rrule_str =
        Option.map
          (fun r ->
            let buf = Buffer.create 128 in
            recurs_to_ics r buf;
            Printf.sprintf "%s: %s\n" "Reccurence" (Buffer.contents buf))
          (get_recurrence event)
        |> Option.value ~default:""
      in
      let summary_str = format_opt "Summary" Fun.id (get_summary event) in
      Printf.sprintf "%s%s%s%s%s%s" summary_str start_str end_str location_str
        description_str rrule_str
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
            ("start", `String (format_datetime ?tz start));
            ( "end",
              match end_ with
              | Some e -> `String (format_datetime ?tz e)
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
      let start = format_datetime ?tz start in
      let end_str =
        match end_ with Some e -> format_datetime ?tz e | None -> ""
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
  | `Sexp ->
      let summary =
        match get_summary event with Some summary -> summary | None -> ""
      in
      let start_date, start_time =
        let dt = Date.ptime_to_timedesc ?tz start in
        let y = Timedesc.year dt in
        let m = Timedesc.month dt in
        let d = Timedesc.day dt in
        let h = Timedesc.hour dt in
        let min = Timedesc.minute dt in
        let s = Timedesc.second dt in
        let dow =
          match Timedesc.weekday dt with
          | `Mon -> "monday"
          | `Tue -> "tuesday"
          | `Wed -> "wednesday"
          | `Thu -> "thursday"
          | `Fri -> "friday"
          | `Sat -> "saturday"
          | `Sun -> "sunday"
        in
        ( Printf.sprintf "(%04d %02d %02d %s)" y m d dow,
          Printf.sprintf "(%02d %02d %02d)" h min s )
      in
      let end_str =
        match end_ with
        | Some end_date ->
            let dt = Date.ptime_to_timedesc ?tz end_date in
            let y = Timedesc.year dt in
            let m = Timedesc.month dt in
            let d = Timedesc.day dt in
            let h = Timedesc.hour dt in
            let min = Timedesc.minute dt in
            let s = Timedesc.second dt in
            let dow =
              match Timedesc.weekday dt with
              | `Mon -> "monday"
              | `Tue -> "tuesday"
              | `Wed -> "wednesday"
              | `Thu -> "thursday"
              | `Fri -> "friday"
              | `Sat -> "saturday"
              | `Sun -> "sunday"
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

let format_event ?(format = `Text) ?tz event =
  format_alt ~format ~start:(Event.get_start event) ~end_:(Event.get_end event)
    ?tz event

let format_instance ?(format = `Text) ?tz instance =
  let open Recur in
  format_alt ~format ~start:instance.start ~end_:instance.end_ ?tz
    instance.event

let format_events ?(format = `Text) ?tz events =
  match format with
  | `Json ->
      let json_events =
        List.map
          (fun e -> Yojson.Safe.from_string (format_event ~format:`Json ?tz e))
          events
      in
      Yojson.Safe.to_string (`List json_events)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n" (List.map (format_event ~format:`Csv ?tz) events)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_event ~format:`Sexp ?tz e) events)
      ^ ")"
  | _ ->
      String.concat "\n" (List.map (fun e -> format_event ~format ?tz e) events)

let format_instances ?(format = `Text) ?tz instances =
  match format with
  | `Json ->
      let json_instances =
        List.map
          (fun e ->
            Yojson.Safe.from_string (format_instance ~format:`Json ?tz e))
          instances
      in
      Yojson.Safe.to_string (`List json_instances)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n"
          (List.map (format_instance ~format:`Csv ?tz) instances)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_instance ~format:`Sexp ?tz e) instances)
      ^ ")"
  | _ ->
      String.concat "\n"
        (List.map (fun e -> format_instance ~format ?tz e) instances)
