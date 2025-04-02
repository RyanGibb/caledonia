open Icalendar

type instance = { event : Event.t; start : Ptime.t; end_ : Ptime.t option }

module Instance = struct
  type t = instance
  type comparator = t -> t -> int

  let by_start i1 i2 = Ptime.compare i1.start i2.start

  let by_end i1 i2 =
    match (i1.end_, i2.end_) with
    | Some t1, Some t2 -> Ptime.compare t1 t2
    | Some _, None -> 1
    | None, Some _ -> -1
    | None, None -> 0

  let by_event event_comp i1 i2 = event_comp i1.event i2.event
  let descending comp i1 i2 = -1 * comp i1 i2

  let chain comp1 comp2 i1 i2 =
    let result = comp1 i1 i2 in
    if result <> 0 then result else comp2 i1 i2
end

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
      (* Include the original event instance only if it falls within the query range. *)
      let start = Event.get_start event in
      let end_ = match Event.get_end event with None -> start | Some e -> e in
      if
        Ptime.compare start to_ < 0
        &&
        (* end_ > f, meaning we don't include events that end at the exact start of our range.
           This is handy to exclude date events that end at 00:00 the next day. *)
        match from with Some f -> Ptime.compare end_ f > 0 | None -> true
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

let combine_results (results : ('a, 'b) result list) : ('a list, 'b) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | Ok v :: rest -> aux (v :: acc) rest
    | Error e :: _ -> Error e
  in
  aux [] results

let parse_recurrence recur =
  let ( let* ) = Result.bind in
  let parts = String.split_on_char ';' recur in
  let freq = ref None in
  let count = ref None in
  let until = ref None in
  let interval = ref None in
  let by_parts = ref [] in
  let results =
    List.map
      (fun part ->
        let kv = String.split_on_char '=' part in
        match kv with
        | [ "FREQ"; value ] -> (
            match String.uppercase_ascii value with
            | "DAILY" ->
                freq := Some `Daily;
                Ok ()
            | "WEEKLY" ->
                freq := Some `Weekly;
                Ok ()
            | "MONTHLY" ->
                freq := Some `Monthly;
                Ok ()
            | "YEARLY" ->
                freq := Some `Yearly;
                Ok ()
            | _ -> Error (`Msg ("Unsupported frequency: " ^ value)))
        | [ "COUNT"; value ] ->
            if !until <> None then
              Error (`Msg "Cannot use both COUNT and UNTIL in the same rule")
            else (
              count := Some (`Count (int_of_string value));
              Ok ())
        | [ "UNTIL"; value ] -> (
            if !count <> None then
              Error (`Msg "Cannot use both COUNT and UNTIL in the same rule")
            else
              let* v =
                match parse_datetime value with
                | Ok v -> Ok v
                | Error e -> Error (`Msg e)
              in
              match v with
              | `With_tzid _ -> Error (`Msg "Until can't be in a timezone")
              | `Utc u ->
                  until := Some (`Until (`Utc u));
                  Ok ()
              | `Local l ->
                  until := Some (`Until (`Local l));
                  Ok ())
        | [ "INTERVAL"; value ] ->
            interval := Some (int_of_string value);
            Ok ()
        | [ "BYDAY"; value ] ->
            (* Parse day specifications like MO,WE,FR or 1MO,-1FR *)
            let days = String.split_on_char ',' value in
            let parse_day day =
              (* Extract ordinal if present (like 1MO or -1FR) *)
              let ordinal, day_code =
                if
                  String.length day >= 3
                  && (String.get day 0 = '+'
                     || String.get day 0 = '-'
                     || (String.get day 0 >= '0' && String.get day 0 <= '9'))
                then (
                  let idx = ref 0 in
                  while
                    !idx < String.length day
                    && (String.get day !idx = '+'
                       || String.get day !idx = '-'
                       || String.get day !idx >= '0'
                          && String.get day !idx <= '9')
                  do
                    incr idx
                  done;
                  let ord_str = String.sub day 0 !idx in
                  let day_str =
                    String.sub day !idx (String.length day - !idx)
                  in
                  (int_of_string ord_str, day_str))
                else (0, day)
              in
              let* weekday =
                match day_code with
                | "MO" -> Ok `Monday
                | "TU" -> Ok `Tuesday
                | "WE" -> Ok `Wednesday
                | "TH" -> Ok `Thursday
                | "FR" -> Ok `Friday
                | "SA" -> Ok `Saturday
                | "SU" -> Ok `Sunday
                | _ -> Error (`Msg ("Invalid weekday: " ^ day_code))
              in
              Ok (ordinal, weekday)
            in
            let* day_specs = combine_results (List.map parse_day days) in
            by_parts := `Byday day_specs :: !by_parts;
            Ok ()
        | [ "BYMONTHDAY"; value ] ->
            let days = String.split_on_char ',' value in
            let month_days = List.map int_of_string days in
            by_parts := `Bymonthday month_days :: !by_parts;
            Ok ()
        | [ "BYMONTH"; value ] ->
            let months = String.split_on_char ',' value in
            let month_nums = List.map int_of_string months in
            by_parts := `Bymonth month_nums :: !by_parts;
            Ok ()
        | _ -> Ok ())
      parts
  in
  let* _ = combine_results results in
  match !freq with
  | Some f ->
      let limit =
        match (!count, !until) with
        | Some c, None -> Some c
        | None, Some u -> Some u
        | _ -> None
      in
      let recurrence = (f, limit, !interval, !by_parts) in
      Ok recurrence
  | None -> Error (`Msg "FREQ is required in recurrence rule")
