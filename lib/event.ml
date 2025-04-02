open Icalendar

type event_id = string
type t = { collection : Collection.t; file_name : string; ical : event }
type date_error = [ `Msg of string ]

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let create ~summary ~start ?end_ ?location ?description ?recurrence collection =
  let uuid = generate_uuid () in
  let uid = (Params.empty, uuid) in
  let file_name = uuid ^ ".ics" in
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
  let ical =
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
  { collection; file_name; ical }

let edit ?summary ?start ?end_ ?location ?description ?recurrence t =
  let now = Ptime_clock.now () in
  let uid = t.ical.uid in
  let dtstart =
    match start with None -> t.ical.dtstart | Some s -> (Params.empty, s)
  in
  let dtend_or_duration =
    match end_ with None -> t.ical.dtend_or_duration | Some _ -> end_
  in
  let rrule =
    match recurrence with
    | None -> t.ical.rrule
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
      t.ical.props
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
  let alarms = t.ical.alarms in
  let ical =
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
  let file_name = t.file_name in
  { collection; file_name; ical }

let of_icalendar collection ~file_name (ical : event) =
  { collection; file_name; ical }

let to_icalendar t =
  let now = Ptime_clock.now () in
  let uid = t.ical.uid in
  let dtstart = t.ical.dtstart in
  let dtend_or_duration = t.ical.dtend_or_duration in
  let rrule = t.ical.rrule in
  let props = t.ical.props in
  let alarms = t.ical.alarms in
  {
    dtstamp = (Params.empty, now);
    uid;
    dtstart;
    dtend_or_duration;
    rrule;
    props;
    alarms;
  }

let get_id t = snd t.ical.uid

let get_summary t =
  match
    List.filter_map
      (function `Summary (_, s) when s <> "" -> Some s | _ -> None)
      t.ical.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_start t = Date.ptime_of_ical (snd t.ical.dtstart)

let get_end t =
  match t.ical.dtend_or_duration with
  | Some (`Dtend (_, d)) -> Some (Date.ptime_of_ical d)
  | Some (`Duration (_, span)) -> (
      let start = get_start t in
      match Ptime.add_span start span with
      | Some t -> Some t
      | None ->
          failwith
            (Printf.sprintf "Invalid duration calculation: %s + %s"
               (Ptime.to_rfc3339 start)
               (Printf.sprintf "%.2fs" (Ptime.Span.to_float_s span))))
  | None -> None

let get_start_timezone t =
  match t.ical.dtstart with
  | _, `Datetime (`With_tzid (_, (_, tzid))) -> Some tzid
  | _ -> None

let get_end_timezone t =
  match t.ical.dtend_or_duration with
  | Some (`Dtend (_, `Datetime (`With_tzid (_, (_, tzid))))) -> Some tzid
  | _ -> None

let get_duration t =
  match t.ical.dtend_or_duration with
  | Some (`Duration (_, span)) -> Some span
  | Some (`Dtend (_, e)) ->
      let span = Ptime.diff (Date.ptime_of_ical e) (get_start t) in
      Some span
  | None -> None

let is_date t =
  match (t.ical.dtstart, t.ical.dtend_or_duration) with
  | (_, `Date _), _ -> true
  | _, Some (`Dtend (_, `Date _)) -> true
  | _ -> false

let get_location t =
  match
    List.filter_map
      (function `Location (_, s) when s <> "" -> Some s | _ -> None)
      t.ical.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_description t =
  match
    List.filter_map
      (function `Description (_, s) when s <> "" -> Some s | _ -> None)
      t.ical.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_recurrence t = Option.map (fun r -> snd r) t.ical.rrule
let get_collection t = t.collection

let get_file_path ~fs ~calendar_dir_path t =
  Eio.Path.(
    fs / calendar_dir_path
    / (match t.collection with Col s -> s)
    / t.file_name)

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
