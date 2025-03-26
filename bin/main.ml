(* Main entry point for the calendar CLI *)

open Cmdliner

let list_cmd = List_cmd.cmd
let search_cmd = Search_cmd.cmd
let doc = "Command-line calendar tool for managing local .ics files"
let version = "%%VERSION%%"

let main env =
  let exit_info =
    [
      Cmd.Exit.info ~doc:"on success." 0;
      Cmd.Exit.info
        ~doc:
          "on error (including invalid date format, file access issues, or \
           other errors)."
        1;
    ]
  in
  let info = Cmd.info "caled" ~version ~doc ~exits:exit_info in
  let default =
    Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))
  in
  let calendar_dir_path =
    match Sys.getenv_opt "CALENDAR_DIR" with
    | Some dir -> dir
    | None -> Filename.concat (Sys.getenv "HOME") ".calendar"
  in
  let fs = Eio.Stdenv.fs env in
  match Caledonia_lib.Calendar_dir.create ~fs calendar_dir_path with
  | Error (`Msg e) ->
      Printf.eprintf "%s" e;
      1
  | Ok calendar_dir -> (
      match
        Cmd.eval_value
          (Cmd.group info ~default
             [ list_cmd ~fs calendar_dir; search_cmd ~fs calendar_dir ])
      with
      | Ok (`Ok n) -> n
      | Ok _ -> 0
      | Error _ -> 1)

let () = Eio_main.run @@ fun env -> exit (main env)
