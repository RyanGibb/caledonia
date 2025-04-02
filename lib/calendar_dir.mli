(** Functions for managing calendar directories with Collection.ts of .ics files
*)

type t
(** A directory of Collection.ts, where each collection is a subdirectory
    containing .ics files *)

val create :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t -> string -> (t, [> `Msg of string ]) result
(** Create a calendar_dir from a directory path. Returns Ok with the
    calendar_dir if successful, or Error with a message if the directory cannot
    be created or accessed. *)

val list_collections :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  (Collection.t list, [> `Msg of string ]) result
(** List available Collection.ts in the calendar_dir. Returns Ok with the list
    of Collection.t names if successful, or Error with a message if the
    directory cannot be read. *)

val get_collection :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  Collection.t ->
  (Event.t list, [> `Msg of string | `Not_found ]) result
(** Get all calendar files in a Collection.t. If the collection doesn't exist in
    the cache, it will be loaded from disk. *)

val get_collections :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  ((Collection.t * Event.t list) list, [> `Msg of string ]) result
(** Get all Collection.ts with their calendar files. This will load any
    Collection.ts that haven't been loaded yet. *)

val add_event :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val edit_event :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val delete_event :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  t ->
  Event.t ->
  (unit, [> `Msg of string ]) result

val get_path : t -> string
