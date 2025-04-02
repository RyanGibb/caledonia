type filter =
  | SummaryContains of string
  | DescriptionContains of string
  | LocationContains of string
  | InCollections of Collection.t list
  | RecurringOnly
  | NonRecurringOnly
  | WithId of Event.event_id
  | And of filter list
  | Or of filter list
  | Not of filter

type sort_order = [ `Ascending | `Descending ]
type sort_by = [ `Start | `End | `Summary | `Location | `Calendar ]

let summary_contains text = SummaryContains text
let description_contains text = DescriptionContains text
let location_contains text = LocationContains text
let in_collections ids = InCollections ids
let recurring_only () = RecurringOnly
let non_recurring_only () = NonRecurringOnly
let with_id id = WithId id
let and_filter filters = And filters
let or_filter filters = Or filters
let not_filter filter = Not filter

let rec matches_filter event = function
  | SummaryContains text -> (
      match Event.get_summary event with
      | Some summary ->
          let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
          Re.Pcre.pmatch ~rex:re summary
      | None -> false)
  | DescriptionContains text -> (
      match Event.get_description event with
      | Some desc ->
          let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
          Re.Pcre.pmatch ~rex:re desc
      | None -> false)
  | LocationContains text -> (
      match Event.get_location event with
      | Some loc ->
          let re = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote text) in
          Re.Pcre.pmatch ~rex:re loc
      | None -> false)
  | InCollections ids ->
      let id = Event.get_collection event in
      List.exists (fun col -> col = id) ids
  | RecurringOnly -> Event.get_recurrence event <> None
  | NonRecurringOnly -> Event.get_recurrence event = None
  | WithId id -> Event.get_id event = id
  | And filters -> List.for_all (matches_filter event) filters
  | Or filters -> List.exists (matches_filter event) filters
  | Not filter -> not (matches_filter event filter)

let compare_events sort_by order e1 e2 =
  let compare =
    match sort_by with
    | `Start ->
        let t1 = Event.get_start e1 in
        let t2 = Event.get_start e2 in
        Ptime.compare t1 t2
    | `End -> (
        match (Event.get_end e1, Event.get_end e2) with
        | Some t1, Some t2 -> Ptime.compare t1 t2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
    | `Summary -> (
        match (Event.get_location e1, Event.get_location e2) with
        | Some l1, Some l2 -> String.compare l1 l2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
    | `Location -> (
        match (Event.get_location e1, Event.get_location e2) with
        | Some l1, Some l2 -> String.compare l1 l2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)
    | `Calendar -> (
        match (Event.get_collection e1, Event.get_collection e2) with
        | Collection.Col c1, Collection.Col c2 -> String.compare c1 c2)
  in
  match order with `Ascending -> compare | `Descending -> -compare

let query_events ~fs calendar_dir ?filter ?sort_by ?order ?limit () =
  let ( let* ) = Result.bind in
  let* collections = Calendar_dir.get_collections ~fs calendar_dir in
  let events =
    List.flatten (List.map (fun (_collection, events) -> events) collections)
  in
  let filtered_events =
    match filter with
    | Some f -> List.filter (fun event -> matches_filter event f) events
    | None -> events
  in
  let sorted_events =
    match (sort_by, order) with
    | Some criteria, Some ord ->
        List.sort (compare_events criteria ord) filtered_events
    | Some criteria, None ->
        List.sort (compare_events criteria `Ascending) filtered_events
    | None, _ -> List.sort (compare_events `Start `Ascending) filtered_events
  in
  Ok
    (match limit with
    | Some n when n > 0 ->
        let rec take n lst acc =
          match (lst, n) with
          | _, 0 -> List.rev acc
          | [], _ -> List.rev acc
          | x :: xs, n -> take (n - 1) xs (x :: acc)
        in
        take n sorted_events []
    | _ -> sorted_events)

let query ~fs calendar_dir ?filter ~from ~to_ ?sort_by ?order ?limit () =
  Fmt.epr "Querying from %a to %a\n%!" Ptime.pp (Option.get from) Ptime.pp to_;
  match query_events ~fs calendar_dir ?filter ?sort_by ?order () with
  | Ok events ->
      let instances =
        List.concat_map
          (fun event -> Recur.expand_event event ~from ~to_)
          events
      in
      let compare_instances criteria ord i1 i2 =
        match criteria with
        | `Start ->
            let c = Ptime.compare i1.Recur.start i2.Recur.start in
            if ord = `Ascending then c else -c
        | `End -> (
            match (i1.Recur.end_, i2.Recur.end_) with
            | Some t1, Some t2 ->
                let c = Ptime.compare t1 t2 in
                if ord = `Ascending then c else -c
            | Some _, None -> if ord = `Ascending then 1 else -1
            | None, Some _ -> if ord = `Ascending then -1 else 1
            | None, None -> 0)
        | other ->
            let c = compare_events other ord i1.Recur.event i2.Recur.event in
            if ord = `Ascending then c else -c
      in
      let sorted_instances =
        match (sort_by, order) with
        | Some criteria, Some ord ->
            List.sort (compare_instances criteria ord) instances
        | Some criteria, None ->
            List.sort (compare_instances criteria `Ascending) instances
        | None, _ -> List.sort (compare_instances `Start `Ascending) instances
      in
      Ok
        (match limit with
        | Some n when n > 0 ->
            let rec take n lst acc =
              match (lst, n) with
              | _, 0 -> List.rev acc
              | [], _ -> List.rev acc
              | x :: xs, n -> take (n - 1) xs (x :: acc)
            in
            take n sorted_instances []
        | _ -> sorted_instances)
  | Error e -> Error e
