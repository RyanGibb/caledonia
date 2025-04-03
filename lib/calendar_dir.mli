(** Functions for managing calendar directories with strings of .ics files *)

type t
(** A directory of strings, where each calendar_name is a subdirectory
    containing .ics files *)

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t -> string -> (t, [> `Msg of string ]) result
(** Create a calendar_dir from a directory path. Returns Ok with the
    calendar_dir if successful, or Error with a message if the directory cannot
    be created or accessed. *)

val list_calendar_names :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (string list, [> `Msg of string ]) result
(** List available strings in the calendar_dir. Returns Ok with the list of
    string names if successful, or Error with a message if the directory cannot
    be read. *)

val get_calendar_events :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (Event.t list, [> `Msg of string | `Not_found ]) result
(** Get all calendar files in a string. If the calendar_name doesn't exist in
    the cache, it will be loaded from disk. *)

val get_events :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (Event.t list, [> `Msg of string ]) result
(** Get all events in all calendar_names. This will load any strings that
    haven't been loaded yet. *)

val add_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val edit_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val delete_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val get_path : t -> string
