open Icalendar

type event_id = string

type t = {
  id : event_id;
  summary : string;
  start : Ptime.t;
  end_ : Ptime.t option;
  location : string option;
  description : string option;
  recurrence : recurrence option;
  collection : Calendar_dir.collection option;
}

type date_error = [ `Msg of string ]

let ptime_of_datetime = function
  | `Datetime (`Utc t) -> Ok t
  | `Datetime (`Local t) -> Ok t
  (* TODO handle timezones *)
  | `Datetime (`With_tzid (t, _)) -> Ok t
  | `Date date -> (
      match Ptime.of_date_time (date, ((0, 0, 0), 0)) with
      | Some t -> Ok t
      | None ->
          let year, month, day = date in
          Error (`Msg (Printf.sprintf "Invalid date %d-%d-%d" year month day)))

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let create ?collection ~summary ~start ?end_ ?location ?description ?recurrence
    () =
  {
    id = generate_uuid ();
    summary;
    start;
    end_;
    location;
    description;
    recurrence;
    collection;
  }

let of_icalendar collection ical_event =
  let ( let* ) = Result.bind in
  let* start = ptime_of_datetime (snd ical_event.dtstart) in
  let* end_ =
    match ical_event.dtend_or_duration with
    | Some (`Dtend (_, dt)) -> (
        match ptime_of_datetime dt with
        | Ok t -> Ok (Some t)
        | Error msg -> Error msg)
    | Some (`Duration (_, span)) -> (
        match Ptime.add_span start span with
        | Some t -> Ok (Some t)
        | None ->
            Error
              (`Msg
                 (Printf.sprintf "Invalid duration calculation: %s + %s"
                    (Ptime.to_rfc3339 start)
                    (Printf.sprintf "%.2fs" (Ptime.Span.to_float_s span)))))
    | None -> Ok None
  in

  let recurrence = Option.map snd ical_event.rrule in
  let uid_ref = ref None in
  let summary_ref = ref None in
  let location_ref = ref None in
  let description_ref = ref None in
  (* TODO we might want to consider using more properties *)
  List.iter
    (function
      | `Summary (_, s) when s <> "" -> summary_ref := Some s
      | `Location (_, s) -> location_ref := Some s
      | `Description (_, s) -> description_ref := Some s
      | `Uid (_, s) when s <> "" -> uid_ref := Some s
      | _ -> ())
    ical_event.props;
  (* TODO these should probably be required *)
  let summary =
    match !summary_ref with Some s -> s | None -> "Untitled Event"
  in
  let id = match !uid_ref with Some s -> s | None -> generate_uuid () in
  Ok
    {
      id;
      summary;
      start;
      end_;
      location = !location_ref;
      description = !description_ref;
      recurrence;
      collection = Some collection;
    }

(* TODO retain unused parameters *)
let to_icalendar t =
  let now = Ptime_clock.now () in
  let props = [] in
  let props = `Summary (Params.empty, t.summary) :: props in
  let props =
    match t.location with
    | Some loc -> `Location (Params.empty, loc) :: props
    | None -> props
  in
  let props =
    match t.description with
    | Some desc -> `Description (Params.empty, desc) :: props
    | None -> props
  in
  {
    dtstamp = (Params.empty, now);
    uid = (Params.empty, t.id);
    dtstart = (Params.empty, `Datetime (`Utc t.start));
    dtend_or_duration =
      Option.map (fun e -> `Dtend (Params.empty, `Datetime (`Utc e))) t.end_;
    rrule = Option.map (fun r -> (Params.empty, r)) t.recurrence;
    props;
    alarms = [];
  }

let get_id t = t.id
let get_summary t = t.summary
let get_start t = t.start
let get_end t = t.end_

let get_duration t =
  match t.end_ with
  | Some e ->
      let span = Ptime.diff e t.start in
      Some span
  | None -> None

let get_location t = t.location
let get_description t = t.description
let get_recurrence t = t.recurrence
let get_collection t = t.collection
