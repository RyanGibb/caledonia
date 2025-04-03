type filter = Event.t -> bool

let text_matches pattern text =
  let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote pattern) in
  Re.Pcre.pmatch ~rex:re text

let summary_contains text event =
  match Event.get_summary event with
  | Some summary -> text_matches text summary
  | None -> false

let description_contains text event =
  match Event.get_description event with
  | Some desc -> text_matches text desc
  | None -> false

let location_contains text event =
  match Event.get_location event with
  | Some loc -> text_matches text loc
  | None -> false

let in_collections ids event =
  let id = Event.get_collection event in
  List.exists (fun col -> col = id) ids

let recurring_only () event = Event.get_recurrence event <> None
let non_recurring_only () event = Event.get_recurrence event = None
let with_id id event = Event.get_id event = id
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

let ( let* ) = Result.bind

let query_without_recurrence ~fs calendar_dir ?filter
    ?(comparator = Event.by_start) ?limit () =
  let* events = Calendar_dir.get_events ~fs calendar_dir in
  let filtered_events =
    match filter with Some f -> List.filter f events | None -> events
  in
  let sorted_events = List.sort comparator filtered_events in
  Ok
    (match limit with
    | Some n when n > 0 -> take n sorted_events
    | _ -> sorted_events)

let query ~fs calendar_dir ?filter ~from ~to_ ?(comparator = Event.by_start)
    ?limit () =
  let* events = Calendar_dir.get_events ~fs calendar_dir in
  let events =
    match filter with Some f -> List.filter f events | None -> events
  in
  let events =
    List.concat_map
      (fun event -> Event.expand_recurrences event ~from ~to_)
      events
  in
  let sorted_events = List.sort comparator events in
  Ok
    (match limit with Some n when n > 0 -> take n events | _ -> sorted_events)
