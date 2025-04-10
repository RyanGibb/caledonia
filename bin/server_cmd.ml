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
              Ok (sexp_of_response (Ok (Calendars names)))
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
