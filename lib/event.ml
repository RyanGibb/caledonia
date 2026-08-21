open Icalendar

type event_id = string

type t = {
  calendar_name : string;
  file : Eio.Fs.dir_ty Eio.Path.t;
  event : event;
  calendar : calendar;
}

type date_error = [ `Msg of string ]

let uuid_gen = lazy (Uuidm.v4_gen (Random.State.make_self_init ()))
let generate_uuid () = Uuidm.to_string (Lazy.force uuid_gen ())

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")
let ( let* ) = Result.bind

let create ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~calendar_dir_path ~summary ~start
    ?end_ ?location ?description ?categories ?recurrence ?(alarms = []) calendar_name =
  let uuid = generate_uuid () in
  let uid = (Params.empty, uuid) in
  let file_name = uuid ^ ".ics" in
  let file =
    Eio.Path.(
      fs / calendar_dir_path / (match calendar_name with s -> s) / file_name)
  in
  let dtstart = start in
  let dtend_or_duration = end_ in
  let* _ =
    match (dtstart, dtend_or_duration) with
    | (_, `Date _), Some (`Dtend (_, `Datetime _)) ->
        Error (`Msg "If the start is a date the end must also be a date.")
    | (_, `Datetime _), Some (`Dtend (_, `Date _)) ->
        Error
          (`Msg "If the start is a datetime the end must also be a datetime.")
    | _ -> Ok ()
  in
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
  let props =
    match categories with
    | Some cats when cats <> [] -> `Categories (Params.empty, cats) :: props
    | _ -> props
  in
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
  let calendar =
    let props = [ default_prodid ] in
    let components = [ `Event event ] in
    (props, components)
  in
  Ok { calendar_name; file; event; calendar }

let edit ?summary ?start ?end_ ?location ?description ?categories ?recurrence ?alarms t =
  let now = Ptime_clock.now () in
  let uid = t.event.uid in
  let dtstart = match start with None -> t.event.dtstart | Some s -> s in
  let dtend_or_duration =
    match end_ with None -> t.event.dtend_or_duration | Some _ -> end_
  in
  let* _ =
    match (dtstart, dtend_or_duration) with
    | (_, `Date _), Some (`Dtend (_, `Datetime _)) ->
        Error (`Msg "If the start is a date the end must also be a date.")
    | (_, `Datetime _), Some (`Dtend (_, `Date _)) ->
        Error
          (`Msg "If the start is a datetime the end must also be a datetime.")
    | _ -> Ok ()
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
        | `Categories _ -> ( match categories with None -> true | Some _ -> false)
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
  let props =
    match categories with
    | Some cats when cats <> [] -> `Categories (Params.empty, cats) :: props
    | _ -> props
  in
  let alarms = match alarms with Some a -> a | None -> t.event.alarms in
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
  let calendar_name = t.calendar_name in
  let file = t.file in
  let calendar = t.calendar in
  Ok { calendar_name; file; event; calendar }

let events_of_icalendar calendar_name ~file calendar =
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
  List.map (function event -> { calendar_name; file; event; calendar }) events

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
          Printf.eprintf
            "Warning: invalid duration %.2fs on event starting %s, ignoring \
             end time\n\
             %!"
            (Ptime.Span.to_float_s span)
            (Ptime.to_rfc3339 start);
          None)
  | None -> None

let get_end t = get_ical_end t.event

let get_start_timezone t =
  match t.event.dtstart with
  | _, `Datetime (`With_tzid (_, (_, tzid))) -> Some tzid
  | _, `Datetime (`Utc _) -> Some "UTC"
  | _ -> None

let get_end_timezone t =
  match t.event.dtend_or_duration with
  | Some (`Dtend (_, `Datetime (`With_tzid (_, (_, tzid))))) -> Some tzid
  | Some (`Dtend (_, `Datetime (`Utc _))) -> Some "UTC"
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

let get_start_civil_date t =
  match snd t.event.dtstart with `Date d -> Some d | _ -> None

let get_end_civil_date t =
  match t.event.dtend_or_duration with
  | Some (`Dtend (_, `Date d)) -> Some d
  | _ -> None

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

let get_categories t =
  List.filter_map
    (function `Categories (_, cats) -> Some cats | _ -> None)
    t.event.props
  |> List.flatten

let get_recurrence t = Option.map (fun r -> snd r) t.event.rrule
let get_alarms t = t.event.alarms
let get_calendar_name t = t.calendar_name
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

let by_calendar_name e1 e2 =
  match (get_calendar_name e1, get_calendar_name e2) with
  | c1, c2 -> String.compare c1 c2

let descending comp e1 e2 = -1 * comp e1 e2

let chain comp1 comp2 e1 e2 =
  let result = comp1 e1 e2 in
  if result <> 0 then result else comp2 e1 e2

(* Resolve a stored TZID to a timezone, if it names one we know. *)
let resolve_tz tzid = Timedesc.Time_zone.make tzid

(* Naive ISO 8601, no offset: the reader is expected to pair it with the
   accompanying *_tz field. Used by the sexp protocol. *)
let format_ptime_iso ?tz p =
  let dt =
    match tz with
    | Some tz -> Date.ptime_to_timedesc ~tz p
    | None -> Date.ptime_to_timedesc p
  in
  let y = Timedesc.year dt in
  let m = Timedesc.month dt in
  let d = Timedesc.day dt in
  let h = Timedesc.hour dt in
  let min = Timedesc.minute dt in
  let s = Timedesc.second dt in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" y m d h min s

(* Offset-qualified RFC 3339, self-contained: an instant that needs no
   accompanying zone to be read correctly. Used by the JSON and CSV exports. *)
let format_ptime_rfc3339 ~tz p =
  Timedesc.to_rfc3339 ~frac_s:0 (Date.ptime_to_timedesc ~tz p)

(* ISO 8601 duration, e.g. -PT3H for an alarm three hours before. *)
let format_iso_duration span =
  let secs = Ptime.Span.to_float_s span in
  let sign = if secs < 0.0 then "-" else "" in
  let total = int_of_float (Float.abs secs) in
  let days = total / 86400 in
  let rem = total mod 86400 in
  let unit n suffix = if n > 0 then string_of_int n ^ suffix else "" in
  let date_part = unit days "D" in
  let time_part =
    unit (rem / 3600) "H" ^ unit (rem mod 3600 / 60) "M" ^ unit (rem mod 60) "S"
  in
  if date_part = "" && time_part = "" then "PT0S"
  else
    sign ^ "P" ^ date_part
    ^ if time_part = "" then "" else "T" ^ time_part

let sexp_of_t event =
  let open Sexplib.Sexp in
  let start = get_start event in
  let end_ = get_end event in
  let start_tz_str = get_start_timezone event in
  let end_tz_str = get_end_timezone event in
  let start_tz = Option.bind start_tz_str resolve_tz in
  let end_tz = Option.bind end_tz_str resolve_tz in
  let entries =
    [
      Some (List [ Atom "id"; Atom (get_id event) ]);
      (match get_summary event with
      | Some s -> Some (List [ Atom "summary"; Atom s ])
      | None -> None);
      Some (List [ Atom "start"; Atom (format_ptime_iso ?tz:start_tz start) ]);
      Some (List [ Atom "start_local"; Atom (format_ptime_iso start) ]);
      (match start_tz_str with
      | Some tz -> Some (List [ Atom "start_tz"; Atom tz ])
      | None -> None);
      (match end_ with
      | Some e -> Some (List [ Atom "end"; Atom (format_ptime_iso ?tz:end_tz e) ])
      | None -> None);
      (match end_ with
      | Some e -> Some (List [ Atom "end_local"; Atom (format_ptime_iso e) ])
      | None -> None);
      (match end_tz_str with
      | Some tz -> Some (List [ Atom "end_tz"; Atom tz ])
      | None -> None);
      (match get_location event with
      | Some l -> Some (List [ Atom "location"; Atom l ])
      | None -> None);
      (match get_description event with
      | Some d -> Some (List [ Atom "description"; Atom d ])
      | None -> None);
      (match get_alarms event with
      | [] -> None
      | alarms -> Some (List [ Atom "alarms"; Atom (Format_utils.format_alarms_short alarms) ]));
      (if is_date event then Some (List [ Atom "is_date"; Atom "true" ])
       else None);
      (* ~tz_offset_s:0 so this reads as UTC; Ptime's default renders the
         offset as -00:00, which RFC 3339 defines as "offset unknown". *)
      Some
        (List
           [ Atom "start_utc"; Atom (Ptime.to_rfc3339 ~tz_offset_s:0 start) ]);
      (match get_recurrence event with
      | Some _ -> Some (List [ Atom "recurring"; Atom "true" ])
      | None -> None);
      Some (List [ Atom "file"; Atom (snd (get_file event)) ]);
      Some (List [ Atom "calendar"; Atom (get_calendar_name event) ]);
    ]
  in
  let filtered_entries = List.filter_map Fun.id entries in
  List filtered_entries

let clone_with_event t event =
  let calendar_name = t.calendar_name in
  let file = t.file in
  let calendar = t.calendar in
  { calendar_name; file; event; calendar }

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

let weekday_string = function
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"

let format_date ?tz date =
  let dt = Date.ptime_to_timedesc ?tz date in
  let y = Timedesc.year dt in
  let m = Timedesc.month dt in
  let d = Timedesc.day dt in
  Printf.sprintf "%04d-%02d-%02d %s" y m d
    (weekday_string (Timedesc.weekday dt))

(* All-day events carry a civil date, not an instant; format it directly so
   the displayed date cannot shift with the display timezone *)
let format_civil_date (y, m, d) =
  match Timedesc.Date.Ymd.make ~year:y ~month:m ~day:d with
  | Ok date ->
      Printf.sprintf "%04d-%02d-%02d %s" y m d
        (weekday_string (Timedesc.Date.weekday date))
  | Error _ -> Printf.sprintf "%04d-%02d-%02d" y m d

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

let day_diff day ~next =
  let span = Ptime.diff next day in
  let d, _ = Ptime.Span.to_d_ps span in
  d

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
  (* RFC 5545 requires UNTIL to be in UTC when DTSTART is timezone-aware *)
  | `With_tzid _ as ts -> datetime_to_str (Date.ptime_of_ical (`Datetime ts)) true

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

let text_event_data ?tz event =
  let id = get_id event in
  let start = get_start event in
  let end_ = get_end event in
  let start_date =
    match get_start_civil_date event with
    | Some d -> format_civil_date d
    | None -> format_date ?tz start
  in
  let start_timezone = get_start_timezone event in
  let end_timezone = get_end_timezone event in
  let start_time =
    match is_date event with true -> "" | false -> " " ^ format_time ?tz start
  in
  let end_date, end_time =
    match end_ with
    | None -> ("", "")
    | Some end_ -> (
        match is_date event with
        | true -> (
            match day_diff start ~next:end_ <= 1 with
            | true -> ("", "")
            | false ->
                let end_str =
                  match get_end_civil_date event with
                  | Some d -> format_civil_date d
                  | None -> format_date ?tz end_
                in
                (" - " ^ end_str, ""))
        | false -> (
            let time_str = " " ^ format_time ?tz end_ in
            match day_diff start ~next:end_ == 0 with
            | true -> ("", " -" ^ time_str)
            | false -> (" - " ^ format_date ?tz end_, time_str)))
  in
  (* Times above are always rendered in the display timezone, so the column
     stays sortable and comparable across rows. This annotation reports the
     zone(s) the event is *stored* in, and is omitted when they match the
     display timezone and so would add nothing. *)
  let tz_annotation =
    if is_date event then ""
    else
      let differs tzid =
        match tz with
        | Some display -> tzid <> Timedesc.Time_zone.name display
        | None -> true
      in
      let single tzid =
        if differs tzid then Printf.sprintf "  [tz: %s]" tzid else ""
      in
      match (start_timezone, end_timezone) with
      | Some s, Some e when s <> e -> Printf.sprintf "  [tz: %s → %s]" s e
      | Some s, _ -> single s
      | None, Some e -> single e
      | None, None -> ""
  in
  let summary =
    match get_summary event with
    | Some summary when summary <> "" -> summary
    | _ -> ""
  in
  let location =
    match get_location event with
    | Some loc when loc <> "" -> "@" ^ loc
    | _ -> ""
  in
  let alarm_str = Format_utils.format_alarms_short (get_alarms event) in
  let calendar_name = get_calendar_name event in
  let date_time =
    start_date ^ start_time ^ end_date ^ end_time ^ tz_annotation
  in
  (id, calendar_name, date_time, summary, location, alarm_str)

let format_prop_value = function
  | `Related (params, s) ->
      let reltype =
        match Icalendar.Params.find Reltype params with
        | Some `Parent -> "PARENT"
        | Some `Child -> "CHILD"
        | Some `Sibling -> "SIBLING"
        | Some (`Ianatoken t) -> t
        | Some (`Xname (ns, name)) -> ns ^ ":" ^ name
        | None -> "PARENT"
      in
      Some ("Related-To", s ^ " (" ^ reltype ^ ")")
  | `Seq (_, n) -> Some ("Sequence", string_of_int n)
  | `Created (_, t) -> Some ("Created", Ptime.to_rfc3339 t)
  | `Lastmod (_, t) -> Some ("Last-Modified", Ptime.to_rfc3339 t)
  | `Iana_prop ("RELATED", params, value) ->
      let reltype =
        match Icalendar.Params.find Reltype params with
        | Some `Parent -> "PARENT"
        | Some `Child -> "CHILD"
        | Some `Sibling -> "SIBLING"
        | Some (`Ianatoken t) -> t
        | Some (`Xname (ns, name)) -> ns ^ ":" ^ name
        | None -> "PARENT"
      in
      Some ("Related-To", value ^ " (" ^ reltype ^ ")")
  | `Iana_prop (name, _, value) -> Some (name, value)
  | `Xprop ((ns, name), _, value) -> Some (ns ^ ":" ^ name, value)
  | _ -> None

(* Shared by the JSON and CSV exports. For each endpoint it yields the instant
   in the zone the event is stored in, the same instant in the display zone,
   and the stored TZID — so a consumer can recover the local reading of a
   cross-zone event, which a single rendering cannot express. All-day events
   carry a plain date and no zone at all. *)
let export_datetime ?tz event =
  let display = Option.value tz ~default:(Date.local_timezone ()) in
  let endpoint = function
    | `Start ->
        (Some (get_start event), get_start_civil_date event,
          get_start_timezone event)
    | `End -> (get_end event, get_end_civil_date event, get_end_timezone event)
  in
  let iso which =
    let p, civil, tzid = endpoint which in
    match (is_date event, civil, p) with
    | true, Some (y, m, d), _ -> Some (Printf.sprintf "%04d-%02d-%02d" y m d)
    | true, None, Some p -> Some (format_ptime_iso ~tz:display p)
    | false, _, Some p ->
        let zone =
          Option.value (Option.bind tzid resolve_tz) ~default:display
        in
        Some (format_ptime_rfc3339 ~tz:zone p)
    | _, _, None -> None
  in
  let iso_local which =
    let p, _, _ = endpoint which in
    if is_date event then None
    else Option.map (format_ptime_rfc3339 ~tz:display) p
  in
  let iso_tz which =
    let _, _, tzid = endpoint which in
    if is_date event then None else tzid
  in
  (iso, iso_local, iso_tz)

let format_event ?(format = `Text) ?tz event =
  let start = get_start event in
  let end_ = get_end event in
  match format with
  | `Text ->
      let id, calendar_name, date_time, summary, location, alarm_str =
        text_event_data ?tz event
      in
      let alarm_part = if alarm_str = "" then "" else "\t" ^ alarm_str in
      Printf.sprintf "%s\t%s\t%s\t%s\t%s%s" calendar_name date_time summary
        location id alarm_part
  | `Entries ->
      let format_opt label f opt =
        Option.map (fun x -> Printf.sprintf "%s: %s\n" label (f x)) opt
        |> Option.value ~default:""
      in
      let start_timezone = get_start_timezone event in
      let end_timezone = get_end_timezone event in
      (* Unlike `list`, which shows a whole column and so needs one common
         frame, `show` renders a single event: the zone it actually happens in
         is the useful one. Each endpoint is therefore rendered in its own
         zone, with the local reading demoted to a suffix when it differs. *)
      let format timezone civil datetime =
        match is_date event with
        | true -> (
            match civil with
            | Some d -> format_civil_date d
            | None -> format_date ?tz datetime)
        | false -> (
            let local = Option.value tz ~default:(Date.local_timezone ()) in
            match Option.bind timezone resolve_tz with
            | Some event_tz
              when Timedesc.Time_zone.name event_tz
                   <> Timedesc.Time_zone.name local ->
                Printf.sprintf "%s  [local: %s]"
                  (format_datetime ~tz:event_tz datetime)
                  (format_datetime ~tz:local datetime)
            | _ -> format_datetime ~tz:local datetime)
      in
      let start_str =
        format_opt "Start"
          (fun d -> format start_timezone (get_start_civil_date event) d)
          (Some start)
      in
      let end_str =
        format_opt "End"
          (fun d -> format end_timezone (get_end_civil_date event) d)
          end_
      in
      (* Own-zone rendering can make a cross-zone event look longer or shorter
         than it is (a 6h flight landing "the next morning"), so state it. *)
      let duration_str =
        match (end_, is_date event) with
        | Some end_, false ->
            let secs = Ptime.Span.to_float_s (Ptime.diff end_ start) in
            if secs <= 0.0 then ""
            else
              let mins = int_of_float (secs /. 60.0) in
              let h = mins / 60 and m = mins mod 60 in
              let parts =
                (if h > 0 then [ Printf.sprintf "%dh" h ] else [])
                @ if m > 0 then [ Printf.sprintf "%dm" m ] else []
              in
              Printf.sprintf "Duration: %s\n" (String.concat " " parts)
        | _ -> ""
      in
      let location_str = format_opt "Location" Fun.id (get_location event) in
      let description_str =
        format_opt "Description" Fun.id (get_description event)
      in
      let rrule_str =
        Option.map
          (fun r ->
            let buf = Buffer.create 128 in
            recurs_to_ics r buf;
            Printf.sprintf "%s: %s\n" "Recurrence" (Buffer.contents buf))
          (get_recurrence event)
        |> Option.value ~default:""
      in
      let summary_str = format_opt "Summary" Fun.id (get_summary event) in
      let alarms_str =
        let alarms = get_alarms event in
        match alarms with
        | [] -> ""
        | _ -> Printf.sprintf "Alarms: %s\n" (Format_utils.format_alarms alarms)
      in
      let other_props_str =
        List.filter_map format_prop_value event.event.props
        |> List.map (fun (name, value) -> Printf.sprintf "%s: %s\n" name value)
        |> String.concat ""
      in
      let file_str = format_opt "File" Fun.id (Some (snd (get_file event))) in
      Printf.sprintf "%s%s%s%s%s%s%s%s%s%s" summary_str start_str end_str
        duration_str location_str description_str rrule_str alarms_str
        other_props_str file_str
  | `Json ->
      let open Yojson.Safe in
      let iso, iso_local, iso_tz = export_datetime ?tz event in
      let opt_string = function Some s -> `String s | None -> `Null in
      let json =
        `Assoc
          [
            ("id", `String (get_id event));
            ( "summary",
              match get_summary event with
              | Some summary -> `String summary
              | None -> `Null );
            ("is_date", `Bool (is_date event));
            ("start", opt_string (iso `Start));
            ("start_local", opt_string (iso_local `Start));
            ("start_tz", opt_string (iso_tz `Start));
            ("end", opt_string (iso `End));
            ("end_local", opt_string (iso_local `End));
            ("end_tz", opt_string (iso_tz `End));
            ( "location",
              match get_location event with
              | Some loc -> `String loc
              | None -> `Null );
            ( "description",
              match get_description event with
              | Some desc -> `String desc
              | None -> `Null );
            ("calendar", match get_calendar_name event with cal -> `String cal);
            ( "alarms",
              `List
                (List.filter_map
                   (fun alarm ->
                     match Format_utils.alarm_trigger alarm with
                     | Some (_, `Duration span) ->
                         Some (`String (format_iso_duration span))
                     | Some (_, `Datetime t) ->
                         Some (`String (Ptime.to_rfc3339 ~tz_offset_s:0 t))
                     | None -> None)
                   (get_alarms event)) );
          ]
      in
      to_string json
  | `Csv ->
      let summary =
        match get_summary event with Some summary -> summary | None -> ""
      in
      let iso, _, iso_tz = export_datetime ?tz event in
      let field = function Some s -> s | None -> "" in
      let location =
        match get_location event with Some loc -> loc | None -> ""
      in
      let cal_id = match get_calendar_name event with cal -> cal in
      Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary
        (field (iso `Start))
        (field (iso_tz `Start))
        (field (iso `End))
        (field (iso_tz `End))
        location cal_id
  | `Ics ->
      let calendar = to_ical_calendar event in
      Icalendar.to_ics ~cr:true calendar
  | `Sexp -> Sexplib.Sexp.to_string (sexp_of_t event)

let format_events_with_dynamic_columns ?tz ?get_color events =
  if events = [] then ""
  else
    let event_data = List.map (text_event_data ?tz) events in
    (* Calculate max width for each column *)
    (* Widths are measured in terminal columns, not bytes: summaries routinely
       contain multi-byte or wide characters, which String.length over-counts
       and so over-pads. *)
    let width = Format_utils.display_width in
    let max_id_width =
      List.fold_left (fun acc (id, _, _, _, _, _) -> max acc (width id)) 0
        event_data
    in
    let max_cal_width =
      List.fold_left
        (fun acc (_, cal, _, _, _, _) -> max acc (width cal))
        0 event_data
    in
    let max_date_width =
      List.fold_left
        (fun acc (_, _, date, _, _, _) -> max acc (width date))
        0 event_data
    in
    (* Calculate max width for summary+location *)
    let max_summary_loc_width =
      List.fold_left
        (fun acc (_, _, _, summary, location, _) ->
          let full_length =
            width summary + if location <> "" then width location + 1 else 0
          in
          max acc full_length)
        0 event_data
    in
    let has_alarms = List.exists (fun (_, _, _, _, _, a) -> a <> "") event_data in
    let max_alarm_width =
      if has_alarms then
        List.fold_left
          (fun acc (_, _, _, _, _, alarm) -> max acc (width alarm))
          0 event_data
      else 0
    in
    (* Format each event with calculated widths *)
    let formatted_events =
      List.map
        (fun (id, cal, date, summary, location, alarm_str) ->
          let color = match get_color with Some f -> f cal | None -> None in
          let summary_loc =
            summary ^ if location <> "" then " " ^ location else ""
          in
          let alarm_col =
            if has_alarms then
              "  " ^ Format_utils.pad_to_width max_alarm_width alarm_str
            else ""
          in
          Printf.sprintf "%s  %s  %s%s  %s"
            (Format_utils.pad_to_width ?color max_cal_width cal)
            (Format_utils.pad_to_width max_date_width date)
            (Format_utils.pad_to_width max_summary_loc_width summary_loc)
            alarm_col
            (Format_utils.pad_to_width max_id_width id))
        event_data
    in
    String.concat "\n" formatted_events

let format_events ?(format = `Text) ?tz ?get_color events =
  match format with
  | `Json ->
      let json_events =
        List.map
          (fun e -> Yojson.Safe.from_string (format_event ~format:`Json ?tz e))
          events
      in
      Yojson.Safe.to_string (`List json_events)
  | `Csv ->
      "\"Summary\",\"Start\",\"Start TZ\",\"End\",\"End TZ\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n" (List.map (format_event ~format:`Csv ?tz) events)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_event ~format:`Sexp ?tz e) events)
      ^ ")"
  | `Text -> format_events_with_dynamic_columns ?tz ?get_color events
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
        recur_events ~recurrence_ids:other_events ical_event
      in
      collect generator []

type filter = t -> bool

let text_matches pattern text =
  let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote pattern) in
  Re.Pcre.pmatch ~rex:re text

let summary_contains text event =
  match get_summary event with
  | Some summary -> text_matches text summary
  | None -> false

let description_contains text event =
  match get_description event with
  | Some desc -> text_matches text desc
  | None -> false

let location_contains text event =
  match get_location event with
  | Some loc -> text_matches text loc
  | None -> false

let in_calendars ids event =
  let id = get_calendar_name event in
  List.exists (fun col -> col = id) ids

let recurring_only () event = get_recurrence event <> None
let non_recurring_only () event = get_recurrence event = None
let with_id id event = get_id event = id
let and_filter filters event = List.for_all (fun filter -> filter event) filters
let or_filter filters event = List.exists (fun filter -> filter event) filters
let not_filter filter event = not (filter event)
let matches_filter event filter = filter event

let take n list =
  let rec aux n lst acc =
    match (lst, n) with
    | _, 0 -> List.rev acc
    | [], _ -> List.rev acc
    | x :: xs, n -> aux (n - 1) xs (x :: acc)
  in
  aux n list []

let query_without_recurrence events ?filter ?(comparator = by_start) ?limit () =
  let events =
    match filter with Some f -> List.filter f events | None -> events
  in
  let events = List.sort comparator events in
  match limit with Some n when n > 0 -> take n events | _ -> events

let query events ?filter ~from ~to_ ?comparator ?limit () =
  let events =
    match filter with Some f -> List.filter f events | None -> events
  in
  let events =
    List.concat_map (fun event -> expand_recurrences event ~from ~to_) events
  in
  let events =
    match comparator with None -> events | Some c -> List.sort c events
  in
  match limit with Some n when n > 0 -> take n events | _ -> events

let utc_to_local_ptime tz occurrence =
  let ts = Timedesc.Utils.timestamp_of_ptime occurrence in
  let dt = Timedesc.of_timestamp_exn ~tz_of_date_time:tz ts in
  let date = Timedesc.date dt in
  let time = Timedesc.time dt in
  let dt_utc = Timedesc.of_date_and_time_exn ~tz:Timedesc.Time_zone.utc date time in
  Date.timedesc_to_ptime dt_utc

let make_timestamp_matching_dtstart dtstart occurrence =
  match dtstart with
  | `Datetime (`Utc _) -> `Utc occurrence
  | `Datetime (`Local _) -> `Local occurrence
  | `Datetime (`With_tzid (_, (params, tzid))) ->
      let tz = match Timedesc.Time_zone.make tzid with
        | Some tz -> tz
        | None -> Timedesc.Time_zone.utc
      in
      `With_tzid (utc_to_local_ptime tz occurrence, (params, tzid))
  | `Date _ -> `Utc occurrence

let make_exdate_value (dtstart : Icalendar.Params.t * Icalendar.date_or_datetime) (occurrence : Ptime.t) =
  match snd dtstart with
  | `Date _ ->
      let date = Ptime.to_date occurrence in
      `Exdate (Icalendar.Params.empty, `Dates [ date ])
  | `Datetime _ ->
      let ts = make_timestamp_matching_dtstart (snd dtstart) occurrence in
      `Exdate (Icalendar.Params.empty, `Datetimes [ ts ])

let delete_occurrence t (occurrence : Ptime.t) =
  let new_ts = make_timestamp_matching_dtstart (snd t.event.dtstart) occurrence in
  (* Merge into existing EXDATE if present, otherwise create new one *)
  let found = ref false in
  let props = List.map (function
    | `Exdate (params, `Datetimes existing) when not !found ->
        found := true;
        `Exdate (params, `Datetimes (existing @ [ new_ts ]))
    | `Exdate (params, `Dates existing) when not !found ->
        found := true;
        `Exdate (params, `Dates (existing @ [ Ptime.to_date occurrence ]))
    | other -> other
  ) t.event.props in
  let props =
    if !found then props
    else make_exdate_value t.event.dtstart occurrence :: props
  in
  let event = { t.event with props } in
  { t with event }

let make_recurrence_id (dtstart : Icalendar.Params.t * Icalendar.date_or_datetime) (occurrence : Ptime.t) =
  match snd dtstart with
  | `Date _ ->
      `Recur_id (Icalendar.Params.empty, `Date (Ptime.to_date occurrence))
  | `Datetime _ ->
      let ts = make_timestamp_matching_dtstart (snd dtstart) occurrence in
      `Recur_id (Icalendar.Params.empty, `Datetime ts)

let create_occurrence_override t (occurrence : Ptime.t) ?summary ?start ?end_ ?location ?description ?alarms () =
  let now = Ptime_clock.now () in
  let recurrence_id = make_recurrence_id t.event.dtstart occurrence in
  let dtstart = match start with None -> t.event.dtstart | Some s -> s in
  let dtend_or_duration =
    match end_ with None -> t.event.dtend_or_duration | Some _ -> end_
  in
  let props = [ recurrence_id ] in
  let props =
    match summary with
    | Some s -> `Summary (Icalendar.Params.empty, s) :: props
    | None ->
        (match List.find_opt (function `Summary _ -> true | _ -> false) t.event.props with
        | Some p -> p :: props
        | None -> props)
  in
  let props =
    match location with
    | Some l -> `Location (Icalendar.Params.empty, l) :: props
    | None ->
        (match List.find_opt (function `Location _ -> true | _ -> false) t.event.props with
        | Some p -> p :: props
        | None -> props)
  in
  let props =
    match description with
    | Some d -> `Description (Icalendar.Params.empty, d) :: props
    | None ->
        (match List.find_opt (function `Description _ -> true | _ -> false) t.event.props with
        | Some p -> p :: props
        | None -> props)
  in
  let alarms = match alarms with Some a -> a | None -> t.event.alarms in
  let override_event : Icalendar.event =
    {
      dtstamp = (Icalendar.Params.empty, now);
      uid = t.event.uid;
      dtstart;
      dtend_or_duration;
      rrule = None;
      props;
      alarms;
    }
  in
  override_event

type alarm_fire = {
  fire_time : Ptime.t;
  event : t;
  alarm : Icalendar.alarm;
}

let compute_alarm_fire_time event alarm =
  match Format_utils.alarm_trigger alarm with
  | Some (_, `Duration span) ->
      Ptime.add_span (get_start event) span
  | Some (_, `Datetime dt) ->
      Some dt
  | None -> None

let compute_alarm_fires ~from ~to_ event =
  let expanded =
    match get_recurrence event with
    | None -> [ event ]
    | Some _ ->
        let buffer_to = match Ptime.add_span to_ (Ptime.Span.of_int_s (7 * 86400)) with
          | Some t -> t
          | None -> to_
        in
        expand_recurrences ~from:None ~to_:buffer_to event
  in
  List.concat_map (fun ev ->
    List.filter_map (fun alarm ->
      match compute_alarm_fire_time ev alarm with
      | Some fire_time ->
          let after_from = match from with
            | None -> true
            | Some f -> Ptime.compare fire_time f >= 0
          in
          let before_to = Ptime.compare fire_time to_ < 0 in
          if after_from && before_to then
            Some { fire_time; event = ev; alarm }
          else None
      | None -> None
    ) (get_alarms ev)
  ) expanded
