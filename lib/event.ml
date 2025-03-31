open Icalendar

type event_id = string
type t = { collection : Collection.t; file_name : string; ical : event }
type date_error = [ `Msg of string ]

(* TODO handle timezones *)
let ptime_of_datetime = function
  | `Datetime (`Utc t) -> t
  | `Datetime (`Local t) -> t
  | `Datetime (`With_tzid (t, _)) -> t
  | `Date date -> (
      match Ptime.of_date date with
      | Some t -> t
      | None ->
          let year, month, day = date in
          failwith (Printf.sprintf "Invalid date %d-%d-%d" year month day))

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let create ~summary ~start ?end_ ?location ?description ?recurrence collection =
  let uuid = generate_uuid () in
  let uid = (Params.empty, uuid) in
  let file_name = uuid ^ ".ics" in
  let dtstart = (Params.empty, `Datetime (`Utc start)) in
  let dtend_or_duration =
    Option.map (fun d -> `Dtend (Params.empty, `Datetime (`Utc d))) end_
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
    match start with
    | None -> t.ical.dtstart
    | Some s -> (Params.empty, `Datetime (`Utc s))
  in
  let dtend_or_duration =
    match end_ with
    | None -> t.ical.dtend_or_duration
    | Some d -> Some (`Dtend (Params.empty, `Datetime (`Utc d)))
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

let get_start t = ptime_of_datetime (snd t.ical.dtstart)

let get_end t =
  match t.ical.dtend_or_duration with
  | Some (`Dtend (_, d)) -> Some (ptime_of_datetime d)
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

let get_duration t =
  match t.ical.dtend_or_duration with
  | Some (`Duration (_, span)) -> Some span
  | Some (`Dtend (_, e)) ->
      let span = Ptime.diff (ptime_of_datetime e) (get_start t) in
      Some span
  | None -> None

let get_day_event t =
  match t.ical.dtstart with _, `Date _ -> true | _ -> false

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
