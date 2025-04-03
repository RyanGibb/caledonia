open Icalendar

type event_id = string

type t = {
  collection : Collection.t;
  file_name : string;
  event : event;
  calendar : calendar;
}

type date_error = [ `Msg of string ]

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")

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
  { collection; file_name; event; calendar }

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
  let file_name = t.file_name in
  let calendar = t.calendar in
  { collection; file_name; event; calendar }

let events_of_icalendar collection ~file_name calendar =
  List.filter_map
    (function
      | `Event event -> Some { collection; file_name; event; calendar }
      | _ -> None)
    (snd calendar)

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

let get_file_path ~fs ~calendar_dir_path t =
  Eio.Path.(
    fs / calendar_dir_path
    / (match t.collection with Col s -> s)
    / t.file_name)

let get_recurrence_ids t =
  let _, recurrence_ids =
    match
      List.partition (function `Event _ -> true | _ -> false) (snd t.calendar)
    with
    | `Event hd :: tl, _ ->
        (hd, List.map (function `Event e -> e | _ -> assert false) tl)
    | _ -> assert false
  in
  recurrence_ids

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
  let file_name = t.file_name in
  let calendar = t.calendar in
  { collection; file_name; event; calendar }

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
        let recurrence_ids = get_recurrence_ids event in
        recur_events ~recurrence_ids ical_event
      in
      collect generator []
