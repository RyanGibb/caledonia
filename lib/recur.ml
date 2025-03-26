open Icalendar

type instance = { event : Event.t; start : Ptime.t; end_ : Ptime.t option }

let clone_with_time original start =
  let duration = Event.get_duration original in
  let end_ =
    match duration with Some span -> Ptime.add_span start span | None -> None
  in
  { event = original; start; end_ }

let expand_event event ~from ~to_ =
  let rule = Event.get_recurrence event in
  match rule with
  (* If there's no recurrence we just return the original event. *)
  | None ->
      let start = Event.get_start event in
      (* Include the original event instance only if it falls within the query range *)
      if
        Ptime.compare start to_ <= 0
        && match from with Some f -> Ptime.compare start f >= 0 | None -> true
      then [ clone_with_time event start ]
      else []
  (* We return all instances within the range, regardless of whether the original
     event instance was included. This ensures recurring events that start before
     the query range but have instances within it are properly included. *)
  | Some rule ->
      let rec collect generator acc =
        match generator () with
        | None -> List.rev acc
        | Some date ->
            if Ptime.compare date to_ > 0 then List.rev acc
            else if
              match from with
              | Some f -> Ptime.compare date f < 0
              | None -> false
            then collect generator acc
            else collect generator (clone_with_time event date :: acc)
      in
      let start_date = Event.get_start event in
      let generator = recur_dates start_date rule in
      collect generator []
