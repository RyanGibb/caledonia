open Cmdliner

let list_cmd = List_cmd.cmd
let search_cmd = Search_cmd.cmd
let show_cmd = Show_cmd.cmd
let add_cmd = Add_cmd.cmd
let delete_cmd = Delete_cmd.cmd
let edit_cmd = Edit_cmd.cmd
let server_cmd = Server_cmd.cmd
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
             [
               list_cmd ~fs calendar_dir;
               search_cmd ~fs calendar_dir;
               show_cmd ~fs calendar_dir;
               add_cmd ~fs calendar_dir;
               edit_cmd ~fs calendar_dir;
               delete_cmd ~fs calendar_dir;
               server_cmd ~stdin:(Eio.Stdenv.stdin env)
                 ~stdout:(Eio.Stdenv.stdout env) ~fs calendar_dir;
             ])
      with
      | Ok (`Ok f) -> f ()
      | Ok _ -> 0
      | Error _ -> 1)

let () = Eio_main.run @@ fun env -> exit (main env)
