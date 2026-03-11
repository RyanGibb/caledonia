open Eio
open Cmdliner
open Caledonia_lib
open Caledonia_lib.Sexp

let run ~stdin ~stdout ~fs calendar_dir () =
  let reader = Buf_read.of_flow stdin ~max_size:1_000_000 in
  let ( let* ) = Result.bind in

  (* Initialize mutable events variable - will be updated on refresh *)
  let mutable_events = ref (Calendar_dir.get_events ~fs calendar_dir) in

  try
    while true do
      let line = Buf_read.line reader in
      let response =
        try
          let sexp = Sexplib.Sexp.of_string line in
          let request = Sexp.request_of_sexp sexp in
          match request with
          | ListCalendars ->
              let* names = Calendar_dir.list_calendar_names ~fs calendar_dir in
              let display_names = List.map (Calendar_dir.get_display_name ~fs calendar_dir) names in
              Ok (sexp_of_response (Ok (Calendars display_names)))
          | Refresh ->
              (* Reload events from disk *)
              mutable_events := Calendar_dir.get_events ~fs calendar_dir;
              (* Return an empty response *)
              Ok (sexp_of_response (Ok Empty))
          | Query query_req ->
              let* filter, from, to_, limit, _tz =
                generate_query_params query_req
              in
              let* events = !mutable_events in
              let events = Event.query events ~filter ~from ~to_ ?limit () in
              Ok (sexp_of_response (Ok (Events events)))
          | CreateEvent req ->
              let calendar_name =
                match Calendar_dir.find_calendar_by_display_name ~fs calendar_dir req.calendar with
                | Some name -> name
                | None -> req.calendar
              in
              let* start = Event_args.parse_start
                  ~start_date:(Some req.start_date) ~start_time:req.start_time
                  ~timezone:req.timezone in
              let* start =
                match start with
                | Some s -> Ok s
                | None -> Error (`Msg "Start date required for events")
              in
              let* end_ =
                let end_date =
                  match (req.end_date, req.end_time) with
                  | None, Some _ -> Some req.start_date
                  | _ -> req.end_date
                in
                let end_date =
                  match end_date with
                  | None -> Some req.start_date
                  | some -> some
                in
                let end_timezone =
                  match (end_date, req.end_time, req.end_timezone) with
                  | Some _, Some _, None -> req.timezone
                  | _ -> req.end_timezone
                in
                Event_args.parse_end ~end_date ~end_time:req.end_time ~end_timezone
              in
              let* recurrence =
                match req.recurrence with
                | Some r ->
                    let* p = Event_args.parse_recurrence r in
                    Ok (Some p)
                | None -> Ok None
              in
              let* alarms = Event_args.parse_alarms req.alarms in
              let* event =
                Event.create ~fs
                  ~calendar_dir_path:(Calendar_dir.get_path calendar_dir)
                  ~summary:req.summary ~start ?end_
                  ?location:req.location ?description:req.description
                  ?recurrence ~alarms calendar_name
              in
              let* events = !mutable_events in
              let* events = Calendar_dir.add_event ~fs calendar_dir events event in
              mutable_events := Ok events;
              Ok (sexp_of_response (Ok (Events [event])))
          | EditEvent req ->
              let* events = !mutable_events in
              let* event =
                match List.filter (fun e -> Event.get_id e = req.id) events with
                | [ e ] -> Ok e
                | [] -> Error (`Msg ("No event found for id " ^ req.id))
                | _ -> Error (`Msg ("Multiple events found for id " ^ req.id))
              in
              let* start = Event_args.parse_start
                  ~start_date:req.start_date ~start_time:req.start_time
                  ~timezone:req.timezone in
              let* end_ =
                let end_date =
                  match (req.end_date, req.end_time) with
                  | None, Some _ -> req.start_date
                  | _ -> req.end_date
                in
                let end_timezone =
                  match (end_date, req.end_time, req.end_timezone) with
                  | Some _, Some _, None -> req.timezone
                  | _ -> req.end_timezone
                in
                Event_args.parse_end ~end_date ~end_time:req.end_time ~end_timezone
              in
              let* recurrence =
                match req.recurrence with
                | Some r ->
                    let* p = Event_args.parse_recurrence r in
                    Ok (Some p)
                | None -> Ok None
              in
              let* alarms_param =
                if req.no_alarms then Ok (Some [])
                else match req.alarms with
                  | [] -> Ok None
                  | strs -> let* a = Event_args.parse_alarms strs in Ok (Some a)
              in
              let* modified = Event.edit ?summary:req.summary ?start ?end_
                  ?location:req.location ?description:req.description
                  ?recurrence ?alarms:alarms_param event in
              let* events = Calendar_dir.edit_event ~fs calendar_dir events modified in
              mutable_events := Ok events;
              Ok (sexp_of_response (Ok (Events [modified])))
          | DeleteEvent event_id ->
              let* events = !mutable_events in
              let* event =
                match List.filter (fun e -> Event.get_id e = event_id) events with
                | [ e ] -> Ok e
                | [] -> Error (`Msg ("No event found for id " ^ event_id))
                | _ -> Error (`Msg ("Multiple events found for id " ^ event_id))
              in
              let* events = Calendar_dir.delete_event ~fs calendar_dir events event in
              mutable_events := Ok events;
              Ok (sexp_of_response (Ok Empty))
        with
        | Sexplib.Conv.Of_sexp_error (_exn, bad_sexp) ->
            let msg =
              Printf.sprintf "Invalid request format for '%s': %s" line
                (to_string bad_sexp)
            in
            Ok (sexp_of_response (Error msg))
        | Failure msg ->
            Ok (sexp_of_response (Error ("Processing failed: " ^ msg)))
        | exn ->
            let msg =
              Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn)
            in
            Ok (sexp_of_response (Error msg))
      in
      let response_line =
        to_string
          (match response with
          | Ok r -> r
          | Error (`Msg msg) -> Sexp.sexp_of_response (Error msg))
      in
      Flow.copy_string (response_line ^ "\n") stdout
    done
  with End_of_file -> ()

let cmd ~stdin ~stdout ~fs calendar_dir =
  let run () =
    let _ = run ~stdin ~stdout ~fs calendar_dir () in
    0
  in
  let term = Term.(const run) in

  let doc = "Process single-line S-expression requests from stdin to stdout." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(mname) $(tname) reads S-expression requests (one per line) from \
         stdin, processes them, and writes S-expression responses (one per \
         line) to stdout.";
      `P "Example request: '(Query (()))'";
      `P
        "Example response: '(Ok (Events ((id ...) (summary ...) ...)))' or \
         '(Error \"...\")'";
      `S Manpage.s_examples;
      `Pre "echo '(Query ((text \\\"meeting\\\")))' | $(mname) $(tname)";
    ]
  in
  let info = Cmd.info "server" ~doc ~man in
  Cmd.v info term
