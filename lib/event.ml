open Icalendar

type event_id = string

type t = {
  collection : Collection.t;
  file : Eio.Fs.dir_ty Eio.Path.t;
  event : event;
  calendar : calendar;
}

type date_error = [ `Msg of string ]

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")

let create ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~calendar_dir_path ~summary ~start
    ?end_ ?location ?description ?recurrence collection =
  let uuid = generate_uuid () in
  let uid = (Params.empty, uuid) in
  let file_name = uuid ^ ".ics" in
  let file =
    Eio.Path.(
      fs / calendar_dir_path
      / (match collection with Collection.Col s -> s)
      / file_name)
  in
  let dtstart = (Params.empty, start) in
  let dtend_or_duration = end_ in
  let rrule = Option.map (fun r -> (Params.empty, r)) recurrence in
  let now = Ptime_clock.now () in
  let props = [ `Summary (Params.empty, summary) ] in
  let props =
    match location with
    | Some loc -> `Location (Params.empty, loc) :: props
    | None -> props
  in
  let props =
    match description with
    | Some desc -> `Description (Params.empty, desc) :: props
    | None -> props
  in
  let event =
    {
      dtstamp = (Params.empty, now);
      uid;
      dtstart;
      dtend_or_duration;
      rrule;
      props;
      alarms = [];
    }
  in
  let calendar =
    let props = [ default_prodid ] in
    let components = [ `Event event ] in
    (props, components)
  in
  { collection; file; event; calendar }

let edit ?summary ?start ?end_ ?location ?description ?recurrence t =
  let now = Ptime_clock.now () in
  let uid = t.event.uid in
  let dtstart =
    match start with None -> t.event.dtstart | Some s -> (Params.empty, s)
  in
  let dtend_or_duration =
    match end_ with None -> t.event.dtend_or_duration | Some _ -> end_
  in
  let rrule =
    match recurrence with
    | None -> t.event.rrule
    | Some r -> Some (Params.empty, r)
  in
  let props =
    List.filter
      (function
        | `Summary _ -> ( match summary with None -> true | Some _ -> false)
        | `Location _ -> ( match location with None -> true | Some _ -> false)
        | `Description _ -> (
            match description with None -> true | Some _ -> false)
        | _ -> true)
      t.event.props
  in
  let props =
    match summary with
    | Some summary -> `Summary (Params.empty, summary) :: props
    | None -> props
  in
  let props =
    match location with
    | Some loc -> `Location (Params.empty, loc) :: props
    | None -> props
  in
  let props =
    match description with
    | Some desc -> `Description (Params.empty, desc) :: props
    | None -> props
  in
  let alarms = t.event.alarms in
  let event =
    {
      dtstamp = (Params.empty, now);
      uid;
      dtstart;
      dtend_or_duration;
      rrule;
      props;
      alarms;
    }
  in
  let collection = t.collection in
  let file = t.file in
  let calendar = t.calendar in
  { collection; file; event; calendar }

let events_of_icalendar collection ~file calendar =
  let remove_dup_ids lst =
    let rec aux acc = function
      | [] -> acc
      | x :: xs ->
          if List.exists (fun r -> r.uid = x.uid) acc then aux acc xs
          else aux (x :: acc) xs
    in
    aux [] lst
  in
  let events =
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
  in
  let events = remove_dup_ids events in
  List.map (function event -> { collection; file; event; calendar }) events

let to_ical_event t = t.event
let to_ical_calendar t = t.calendar
let get_id t = snd t.event.uid

let get_summary t =
  match
    List.filter_map
      (function `Summary (_, s) when s <> "" -> Some s | _ -> None)
      t.event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_ical_start event = Date.ptime_of_ical (snd event.dtstart)
let get_start t = get_ical_start t.event

let get_ical_end event =
  match event.dtend_or_duration with
  | Some (`Dtend (_, d)) -> Some (Date.ptime_of_ical d)
  | Some (`Duration (_, span)) -> (
      let start = get_ical_start event in
      match Ptime.add_span start span with
      | Some t -> Some t
      | None ->
          failwith
            (Printf.sprintf "Invalid duration calculation: %s + %s"
               (Ptime.to_rfc3339 start)
               (Printf.sprintf "%.2fs" (Ptime.Span.to_float_s span))))
  | None -> None

let get_end t = get_ical_end t.event

let get_start_timezone t =
  match t.event.dtstart with
  | _, `Datetime (`With_tzid (_, (_, tzid))) -> Some tzid
  | _ -> None

let get_end_timezone t =
  match t.event.dtend_or_duration with
  | Some (`Dtend (_, `Datetime (`With_tzid (_, (_, tzid))))) -> Some tzid
  | _ -> None

let get_duration t =
  match t.event.dtend_or_duration with
  | Some (`Duration (_, span)) -> Some span
  | Some (`Dtend (_, e)) ->
      let span = Ptime.diff (Date.ptime_of_ical e) (get_start t) in
      Some span
  | None -> None

let is_date t =
  match (t.event.dtstart, t.event.dtend_or_duration) with
  | (_, `Date _), _ -> true
  | _, Some (`Dtend (_, `Date _)) -> true
  | _ -> false

let get_location t =
  match
    List.filter_map
      (function `Location (_, s) when s <> "" -> Some s | _ -> None)
      t.event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_description t =
  match
    List.filter_map
      (function `Description (_, s) when s <> "" -> Some s | _ -> None)
      t.event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_recurrence t = Option.map (fun r -> snd r) t.event.rrule
let get_collection t = t.collection
let get_file t = t.file

type comparator = t -> t -> int

let by_start e1 e2 =
  let t1 = get_start e1 in
  let t2 = get_start e2 in
  Ptime.compare t1 t2

let by_end e1 e2 =
  match (get_end e1, get_end e2) with
  | Some t1, Some t2 -> Ptime.compare t1 t2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let by_summary e1 e2 =
  match (get_summary e1, get_summary e2) with
  | Some s1, Some s2 -> String.compare s1 s2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let by_location e1 e2 =
  match (get_location e1, get_location e2) with
  | Some l1, Some l2 -> String.compare l1 l2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let by_collection e1 e2 =
  match (get_collection e1, get_collection e2) with
  | Collection.Col c1, Collection.Col c2 -> String.compare c1 c2

let descending comp e1 e2 = -1 * comp e1 e2

let chain comp1 comp2 e1 e2 =
  let result = comp1 e1 e2 in
  if result <> 0 then result else comp2 e1 e2

let clone_with_event t event =
  let collection = t.collection in
  let file = t.file in
  let calendar = t.calendar in
  { collection; file; event; calendar }

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

let same_day day other =
  let y1, m1, d1 = Ptime.to_date day in
  let y2, m2, d2 = Ptime.to_date other in
  y1 == y2 && m1 == m2 && d1 == d2

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

let format_event ?(format = `Text) ?tz event =
  let start = get_start event in
  let end_ = get_end event in
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
            match is_date event with
            | true -> (
                match next_day start ~next:end_ with
                | true -> ("", "")
                | false -> (" - " ^ format_date ?tz end_, ""))
            | false -> (
                match same_day start end_ with
                | true -> ("", " - " ^ format_time ?tz end_)
                | false ->
                    (" - " ^ format_date ?tz end_, " " ^ format_time ?tz end_)))
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
      let file_str = format_opt "File" Fun.id (Some (snd (get_file event))) in
      Printf.sprintf "%s%s%s%s%s%s%s" summary_str start_str end_str location_str
        description_str rrule_str file_str
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
      let calendar = to_ical_calendar event in
      Icalendar.to_ics ~cr:true calendar
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

let expand_recurrences ~from ~to_ event =
  let rule = get_recurrence event in
  match rule with
  (* If there's no recurrence we just return the original event. *)
  | None ->
      (* Include the original event instance only if it falls within the query range. *)
      let start = get_start event in
      let end_ = match get_end event with None -> start | Some e -> e in
      if
        Ptime.compare start to_ < 0
        &&
        (* end_ > f, meaning we don't include events that end at the exact start of our range.
           This is handy to exclude date events that end at 00:00 the next day. *)
        match from with Some f -> Ptime.compare end_ f > 0 | None -> true
      then [ event ]
      else []
  | Some _ ->
      let rec collect generator acc =
        match generator () with
        | None -> List.rev acc
        | Some recur ->
            let start = get_ical_start recur in
            let end_ =
              match get_ical_end recur with None -> start | Some e -> e
            in
            (* if start >= to then we're outside our (exclusive) date range and we terminate *)
            if Ptime.compare start to_ >= 0 then List.rev acc
              (* if end > from then, *)
            else if
              match from with
              | Some f -> Ptime.compare end_ f > 0
              | None -> true
            (* we include the event *)
            then collect generator (clone_with_event event recur :: acc)
            (* otherwise we iterate till the event is in range *)
              else collect generator acc
      in
      let generator =
        let ical_event = to_ical_event event in
        (* The first event is the non recurrence-id one *)
        let _, other_events =
          match
            List.partition
              (function `Event _ -> true | _ -> false)
              (snd event.calendar)
          with
          | `Event hd :: tl, _ ->
              (hd, List.map (function `Event e -> e | _ -> assert false) tl)
          | _ -> assert false
        in
        Fmt.pr "Event:\n%s\n"
          (format_event ~format:`Entries (clone_with_event event ical_event));
        List.iter
          (fun e ->
            Fmt.pr "Recurrent ID event:\n%s\n"
              (format_event ~format:`Entries (clone_with_event event e)))
          other_events;
        (* Icalendar filters for equal uids *)
        recur_events ~recurrence_ids:other_events ical_event
      in
      collect generator []
