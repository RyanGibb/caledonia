open Cmdliner
open Caledonia_lib

module Fired_set = Set.Make (struct
  type t = string * string * Ptime.t
  (* (component_id, trigger_str, fire_time) *)

  let compare (id1, tr1, ft1) (id2, tr2, ft2) =
    match String.compare id1 id2 with
    | 0 -> (
        match String.compare tr1 tr2 with
        | 0 -> Ptime.compare ft1 ft2
        | n -> n)
    | n -> n
end)

let compute_next_fires ~tz ~fs calendar_dir =
  let now = Ptime_clock.now () in
  let to_ = Date.add_months now 1 in
  match Calendar_dir.get_components ~fs calendar_dir with
  | Error (`Msg msg) ->
      Printf.eprintf "Error loading components: %s\n%!" msg;
      []
  | Ok components ->
      Component.query_alarm_fires ~from:(Some now) ~to_ components
      |> List.map (fun (af : Component.alarm_fire) ->
             let trigger_str =
               match Format_utils.alarm_trigger af.alarm with
               | Some (_, `Duration span) ->
                   Format_utils.format_alarm_trigger span
               | Some (_, `Datetime _) -> "at fixed time"
               | None -> ""
             in
             let summary =
               match Component.get_summary af.component with
               | Some s -> s
               | None -> "(no summary)"
             in
             let fire_time_str =
               let dt = Date.ptime_to_timedesc ~tz af.fire_time in
               Printf.sprintf "%02d:%02d" (Timedesc.hour dt)
                 (Timedesc.minute dt)
             in
             (af, trigger_str, summary, fire_time_str))

let send_notification ~summary ~trigger_str ~fire_time_str ~calendar =
  let title = Printf.sprintf "%s" summary in
  let body =
    Printf.sprintf "%s — %s (%s)" fire_time_str trigger_str calendar
  in
  let pid =
    Unix.create_process "notify-send" [| "notify-send"; "-u"; "critical"; "-a"; "Caledonia"; title; body |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  ignore (Unix.waitpid [] pid)

let setup_inotify calendar_dir_path =
  let fd = Inotify.create () in
  (* Watch the top-level calendar directory for new subdirs *)
  ignore
    (Inotify.add_watch fd calendar_dir_path
       [ Inotify.S_Create; Inotify.S_Delete; Inotify.S_Modify ]);
  (* Watch each calendar subdirectory for .ics changes *)
  (try
     let entries = Sys.readdir calendar_dir_path in
     Array.iter
       (fun entry ->
         let path = Filename.concat calendar_dir_path entry in
         if Sys.is_directory path && String.length entry > 0 && entry.[0] <> '.'
         then
           ignore
             (Inotify.add_watch fd path
                [
                  Inotify.S_Close_write;
                  Inotify.S_Create;
                  Inotify.S_Delete;
                  Inotify.S_Moved_to;
                  Inotify.S_Moved_from;
                ]))
       entries
   with Sys_error msg ->
     Printf.eprintf "Warning: could not scan calendar subdirectories: %s\n%!"
       msg);
  fd

let run ~clock ~fs calendar_dir () =
  let calendar_dir_path = Calendar_dir.get_path calendar_dir in
  let tz = Date.local_timezone () in
  let fired = ref Fired_set.empty in
  Printf.printf "Alarm daemon started, watching %s\n%!" calendar_dir_path;
  let fire_alarm ~trigger_str ~summary ~fire_time_str af =
    let key =
      ( Component.get_id af.Component.component,
        trigger_str,
        af.Component.fire_time )
    in
    if not (Fired_set.mem key !fired) then (
      let calendar = Component.get_calendar_name af.Component.component in
      send_notification ~summary ~trigger_str ~fire_time_str ~calendar;
      Printf.printf "Fired: %s (%s) - %s\n%!" summary trigger_str
        fire_time_str;
      fired := Fired_set.add key !fired)
  in
  let wait_or_inotify inotify_fd timeout_secs =
    Eio.Fiber.first
      (fun () ->
        Eio_unix.await_readable inotify_fd;
        ignore (Inotify.read inotify_fd);
        `Inotify)
      (fun () ->
        Eio.Time.sleep clock timeout_secs;
        `Timer)
  in
  let rec loop () =
    let fires = compute_next_fires ~tz ~fs calendar_dir in
    let now = Ptime_clock.now () in
    (* Fire any alarms that are already due *)
    let pending =
      List.filter
        (fun (af, trigger_str, summary, fire_time_str) ->
          let key =
            ( Component.get_id af.Component.component,
              trigger_str,
              af.Component.fire_time )
          in
          if Fired_set.mem key !fired then false
          else if Ptime.compare af.Component.fire_time now <= 0 then (
            fire_alarm ~trigger_str ~summary ~fire_time_str af;
            false)
          else true)
        fires
    in
    match pending with
    | [] ->
        Printf.printf "No upcoming alarms, checking again in 1 hour\n%!";
        let inotify_fd = setup_inotify calendar_dir_path in
        let reason = wait_or_inotify inotify_fd 3600.0 in
        Unix.close inotify_fd;
        (match reason with
        | `Inotify ->
            Printf.printf "Calendar changed, recomputing alarms\n%!";
            Eio.Time.sleep clock 0.5
        | `Timer -> ());
        loop ()
    | (next_af, next_trigger, next_summary, next_time_str) :: _ ->
        let diff = Ptime.diff next_af.Component.fire_time now in
        let secs = Ptime.Span.to_float_s diff in
        Printf.printf "Next alarm in %.0f seconds\n%!" secs;
        let sleep_seconds = max 1.0 secs in
        let inotify_fd = setup_inotify calendar_dir_path in
        let reason = wait_or_inotify inotify_fd sleep_seconds in
        Unix.close inotify_fd;
        (match reason with
        | `Timer ->
            fire_alarm ~trigger_str:next_trigger ~summary:next_summary
              ~fire_time_str:next_time_str next_af
        | `Inotify ->
            Printf.printf "Calendar changed, recomputing alarms\n%!";
            Eio.Time.sleep clock 0.5);
        loop ()
  in
  loop ()

let cmd ~clock ~fs calendar_dir =
  let run () = run ~clock ~fs calendar_dir () in
  let term = Term.(const run) in
  let doc = "Run alarm notification daemon" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Run a daemon that monitors calendar files and sends desktop \
         notifications (via notify-send) when alarms fire.";
      `P
        "The daemon watches the calendar directory for changes using inotify \
         and sleeps until the next alarm is due, so it uses no CPU while \
         waiting.";
      `S Manpage.s_examples;
      `I ("Run the alarm daemon:", "caled alarm-daemon");
      `I
        ( "Run in the background:",
          "caled alarm-daemon &" );
    ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "alarm-daemon" ~doc ~man ~exits:exit_info in
  Cmd.v info term
