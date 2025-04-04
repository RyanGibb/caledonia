open Cmdliner
open Caledonia_lib
open Event_args

let run ~event_id ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ?timezone ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let filter = Query.with_id event_id in
  let* results = Query.query_without_recurrence ~fs calendar_dir ~filter () in
  let* event =
    match results with
    | [ event ] -> Ok event
    | [] -> Error (`Msg ("No events found found for id " ^ event_id))
    | _ -> Error (`Msg ("More than one found for id " ^ event_id))
  in
  let* start = parse_start ~start_date ~start_time ~timezone in
  let* end_ =
    let end_date =
      match end_date with None -> start_date | Some e -> Some e
    in
    parse_end ~end_date ~end_time ~timezone ~end_timezone
  in
  let* recurrence =
    match recur with
    | Some r ->
        let* p = parse_recurrence r in
        Ok (Some p)
    | None -> Ok None
  in
  let modifed_event =
    Event.edit ?summary ?start ?end_ ?location ?description ?recurrence event
  in
  let* _ = Calendar_dir.edit_event ~fs calendar_dir modifed_event in
  Printf.printf "Event %s updated.\n" event_id;
  Ok ()

let event_id_arg =
  let doc = "ID of the event to edit" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EVENT_ID" ~doc)

let cmd ~fs calendar_dir =
  let run event_id summary start_date start_time end_date end_time location
      description recur timezone end_timezone =
    match
      run ~event_id ~summary ~start_date ~start_time ~end_date ~end_time
        ~location ~description ~recur ?timezone ?end_timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Printf.eprintf "Error: %s\n%!" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ event_id_arg $ optional_summary_arg $ start_date_arg
      $ start_time_arg $ end_date_arg $ end_time_arg $ location_arg
      $ description_arg $ recur_arg $ timezone_arg $ end_timezone_arg)
  in
  let doc = "Edit an existing calendar event" in
  let man =
    [
      `S Manpage.s_description;
      `P "Edit an existing event in your calendar by its ID.";
      `P
        "Specify the event ID as the first argument, and use options to change \
         event details.";
      `S Manpage.s_examples;
      `I
        ( "Change the summary of an event:",
          "caled edit 12345678-1234-5678-1234-567812345678 --summary \"New \
           Title\"" );
      `I
        ( "Change the date and time:",
          "caled edit 12345678-1234-5678-1234-567812345678 --date 2025-05-01 \
           --time 15:30" );
      `I
        ( "Update the location:",
          "caled edit 12345678-1234-5678-1234-567812345678 --location \
           \"Conference Room B\"" );
      `I
        ( "Change the description:",
          "caled edit 12345678-1234-5678-1234-567812345678 --description \
           \"Updated agenda for the meeting\"" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "edit" ~doc ~man ~exits:exit_info in
  Cmd.v info term
