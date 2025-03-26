type format = [ `Text | `Json | `Csv | `Ics | `Table | `Sexp ]

let format_date date =
  let y, m, d = Ptime.to_date date in
  let cal_date = CalendarLib.Date.make y m d in
  let weekday =
    match CalendarLib.Date.day_of_week cal_date with
    | CalendarLib.Date.Mon -> "Mon"
    | CalendarLib.Date.Tue -> "Tue"
    | CalendarLib.Date.Wed -> "Wed"
    | CalendarLib.Date.Thu -> "Thu"
    | CalendarLib.Date.Fri -> "Fri"
    | CalendarLib.Date.Sat -> "Sat"
    | CalendarLib.Date.Sun -> "Sun"
  in
  Printf.sprintf "%04d-%02d-%02d %s" y m d weekday

let format_time date =
  let _, ((h, m, _), _) = Ptime.to_date_time date in
  Printf.sprintf "%02d:%02d" h m

let format_datetime date =
  Printf.sprintf "%s %s" (format_date date) (format_time date)

let format_event ?(format = `Text) event =
  let open Event in
  match format with
  | `Text ->
      let summary = get_summary event in
      let date = format_date (get_start event) in
      let time = format_time (get_start event) in
      let end_time_str =
        match get_end event with
        | Some e -> Printf.sprintf "-%s" (format_time e)
        | None -> ""
      in
      let location_str =
        match get_location event with
        | Some loc when loc <> "" -> Printf.sprintf " @ %s" loc
        | _ -> ""
      in
      let recur_str =
        match get_recurrence event with
        | Some _ -> Printf.sprintf " (recurring)"
        | None -> ""
      in
      Printf.sprintf "%s %s%s %s%s%s" date time end_time_str summary
        location_str recur_str
  | `Json ->
      let open Yojson.Safe in
      let json =
        `Assoc
          [
            ("id", `String (get_id event));
            ("summary", `String (get_summary event));
            ("start", `String (format_datetime (get_start event)));
            ( "end",
              match get_end event with
              | Some e -> `String (format_datetime e)
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
              | Some (Calendar_dir.Collection cal) -> `String cal
              | None -> `Null );
          ]
      in
      to_string json
  | `Csv ->
      let summary = get_summary event in
      let start = format_datetime (get_start event) in
      let end_str =
        match get_end event with Some e -> format_datetime e | None -> ""
      in
      let location =
        match get_location event with Some loc -> loc | None -> ""
      in
      let cal_id =
        match get_collection event with
        | Some (Calendar_dir.Collection cal) -> cal
        | None -> ""
      in
      Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary start end_str
        location cal_id
  | `Ics ->
      let cal_props = [] in
      let event_ical = Event.to_icalendar event in
      Icalendar.to_ics (cal_props, [ `Event event_ical ])
  | `Table ->
      let width = 80 in
      let hr = String.make width '-' in
      let summary = get_summary event in
      let start = format_datetime (get_start event) in
      let end_str =
        match get_end event with Some e -> format_datetime e | None -> ""
      in
      let location =
        match get_location event with Some loc -> loc | None -> ""
      in
      Printf.sprintf
        "%s\n\
         | %-20s | %-30s |\n\
         | %-20s | %-30s |\n\
         | %-20s | %-30s |\n\
         | %-20s | %-30s |\n\
         %s"
        hr "Summary" summary "Start" start "End" end_str "Location" location hr
  | `Sexp ->
      let summary = get_summary event in
      let start_date, start_time =
        let date = get_start event in
        let y, m, d = Ptime.to_date date in
        let _, ((h, min, s), _) = Ptime.to_date_time date in
        let cal_date = CalendarLib.Date.make y m d in
        let dow =
          match CalendarLib.Date.day_of_week cal_date with
          | CalendarLib.Date.Mon -> "monday"
          | CalendarLib.Date.Tue -> "tuesday"
          | CalendarLib.Date.Wed -> "wednesday"
          | CalendarLib.Date.Thu -> "thursday"
          | CalendarLib.Date.Fri -> "friday"
          | CalendarLib.Date.Sat -> "saturday"
          | CalendarLib.Date.Sun -> "sunday"
        in
        ( Printf.sprintf "(%04d %02d %02d %s)" y m d dow,
          Printf.sprintf "(%02d %02d %02d)" h min s )
      in
      let end_str =
        match get_end event with
        | Some end_date ->
            let y, m, d = Ptime.to_date end_date in
            let _, ((h, min, s), _) = Ptime.to_date_time end_date in
            let cal_date = CalendarLib.Date.make y m d in
            let dow =
              match CalendarLib.Date.day_of_week cal_date with
              | CalendarLib.Date.Mon -> "monday"
              | CalendarLib.Date.Tue -> "tuesday"
              | CalendarLib.Date.Wed -> "wednesday"
              | CalendarLib.Date.Thu -> "thursday"
              | CalendarLib.Date.Fri -> "friday"
              | CalendarLib.Date.Sat -> "saturday"
              | CalendarLib.Date.Sun -> "sunday"
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
        | Some (Calendar_dir.Collection cal) ->
            Printf.sprintf "\"%s\"" (String.escaped cal)
        | None -> "nil"
      in
      let id = get_id event in
      Printf.sprintf
        "((:id \"%s\" :summary \"%s\" :start (%s %s) :end %s :location %s \
         :description %s :calendar %s))"
        (String.escaped id) (String.escaped summary) start_date start_time
        end_str location description calendar

let format_instance ?(format = `Text) instance =
  match format with
  | `Text ->
      let summary = Event.get_summary instance.Recur.event in
      let date = format_date instance.Recur.start in
      let time = format_time instance.Recur.start in
      let end_time_str =
        match instance.Recur.end_ with
        | Some e -> Printf.sprintf "-%s" (format_time e)
        | None -> ""
      in
      let location_str =
        match Event.get_location instance.Recur.event with
        | Some loc when loc <> "" -> Printf.sprintf " @ %s" loc
        | _ -> ""
      in
      Printf.sprintf "%s %s%s %s%s" date time end_time_str summary location_str
  | format -> format_event ~format instance.Recur.event

let format_events ?(format = `Text) events =
  match format with
  | `Json ->
      let json_events =
        List.map
          (fun e -> Yojson.Safe.from_string (format_event ~format:`Json e))
          events
      in
      Yojson.Safe.to_string (`List json_events)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n"
          (List.map
             (fun e ->
               let summary = Event.get_summary e in
               let start = format_datetime (Event.get_start e) in
               let end_str =
                 match Event.get_end e with
                 | Some end_ -> format_datetime end_
                 | None -> ""
               in
               let location =
                 match Event.get_location e with Some loc -> loc | None -> ""
               in
               let cal_id =
                 match Event.get_collection e with
                 | Some (Calendar_dir.Collection cal) -> cal
                 | None -> ""
               in
               Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary start
                 end_str location cal_id)
             events)
  | `Sexp ->
      (* For S-expressions, we want a list of event S-expressions *)
      "("
      ^ String.concat "\n "
          (List.map (fun e -> format_event ~format:`Sexp e) events)
      ^ ")"
  | _ -> String.concat "\n" (List.map (fun e -> format_event ~format e) events)

let format_instances ?(format = `Text) instances =
  match format with
  | `Json ->
      let json_instances =
        List.map
          (fun i ->
            let e = i.Recur.event in
            let json = Yojson.Safe.from_string (format_event ~format:`Json e) in
            match json with
            | `Assoc fields ->
                `Assoc
                  (("start", `String (format_datetime i.Recur.start))
                  :: ( "end",
                       match i.Recur.end_ with
                       | Some e -> `String (format_datetime e)
                       | None -> `Null )
                  :: List.filter
                       (fun (k, _) -> k <> "start" && k <> "end")
                       fields)
            | _ -> json)
          instances
      in
      Yojson.Safe.to_string (`List json_instances)
  | `Csv ->
      "\"Summary\",\"Start\",\"End\",\"Location\",\"Calendar\"\n"
      ^ String.concat "\n"
          (List.map
             (fun i ->
               let e = i.Recur.event in
               let summary = Event.get_summary e in
               let start = format_datetime i.Recur.start in
               let end_str =
                 match i.Recur.end_ with
                 | Some end_ -> format_datetime end_
                 | None -> ""
               in
               let location =
                 match Event.get_location e with Some loc -> loc | None -> ""
               in
               let cal_id =
                 match Event.get_collection e with
                 | Some (Calendar_dir.Collection cal) -> cal
                 | None -> ""
               in
               Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary start
                 end_str location cal_id)
             instances)
  | `Sexp ->
      (* Create a list of instance S-expressions *)
      let format_instance_sexp i =
        let e = i.Recur.event in
        let summary = Event.get_summary e in
        let start_date, start_time =
          let date = i.Recur.start in
          let y, m, d = Ptime.to_date date in
          let _, ((h, min, s), _) = Ptime.to_date_time date in
          let cal_date = CalendarLib.Date.make y m d in
          let dow =
            match CalendarLib.Date.day_of_week cal_date with
            | CalendarLib.Date.Mon -> "monday"
            | CalendarLib.Date.Tue -> "tuesday"
            | CalendarLib.Date.Wed -> "wednesday"
            | CalendarLib.Date.Thu -> "thursday"
            | CalendarLib.Date.Fri -> "friday"
            | CalendarLib.Date.Sat -> "saturday"
            | CalendarLib.Date.Sun -> "sunday"
          in
          ( Printf.sprintf "(%04d %02d %02d %s)" y m d dow,
            Printf.sprintf "(%02d %02d %02d)" h min s )
        in
        let end_str =
          match i.Recur.end_ with
          | Some end_date ->
              let y, m, d = Ptime.to_date end_date in
              let _, ((h, min, s), _) = Ptime.to_date_time end_date in
              let cal_date = CalendarLib.Date.make y m d in
              let dow =
                match CalendarLib.Date.day_of_week cal_date with
                | CalendarLib.Date.Mon -> "monday"
                | CalendarLib.Date.Tue -> "tuesday"
                | CalendarLib.Date.Wed -> "wednesday"
                | CalendarLib.Date.Thu -> "thursday"
                | CalendarLib.Date.Fri -> "friday"
                | CalendarLib.Date.Sat -> "saturday"
                | CalendarLib.Date.Sun -> "sunday"
              in
              Printf.sprintf "((%04d %02d %02d %s) (%02d %02d %02d))" y m d dow
                h min s
          | None -> "nil"
        in
        let location =
          match Event.get_location e with
          | Some loc -> Printf.sprintf "\"%s\"" (String.escaped loc)
          | None -> "nil"
        in
        let description =
          match Event.get_description e with
          | Some desc -> Printf.sprintf "\"%s\"" (String.escaped desc)
          | None -> "nil"
        in
        let calendar =
          match Event.get_collection e with
          | Some (Calendar_dir.Collection cal) ->
              Printf.sprintf "\"%s\"" (String.escaped cal)
          | None -> "nil"
        in
        let id = Event.get_id e in
        Printf.sprintf
          "((:id \"%s\" :summary \"%s\" :start (%s %s) :end %s :location %s \
           :description %s :calendar %s))"
          (String.escaped id) (String.escaped summary) start_date start_time
          end_str location description calendar
      in
      "(" ^ String.concat "\n " (List.map format_instance_sexp instances) ^ ")"
  | _ ->
      String.concat "\n"
        (List.map (fun i -> format_instance ~format i) instances)
