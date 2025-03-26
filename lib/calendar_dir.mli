(** Functions for managing calendar directories with collections of .ics files
*)

type collection = Collection of string  (** The name of the collection. *)

module CollectionMap : Map.S with type key = collection
(** Module for mapping collection names to their calendar files *)

type calendar_file = { calendar : Icalendar.calendar; file_path : string }
(** Record representing a calendar file with its metadata *)

type calendar_dir
(** A directory of collections, where each collection is a subdirectory
    containing .ics files *)

val create :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  string ->
  (calendar_dir, [> `Msg of string ]) result
(** Create a calendar_dir from a directory path. Returns Ok with the
    calendar_dir if successful, or Error with a message if the directory cannot
    be created or accessed. *)

val list_collections :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  calendar_dir ->
  (collection list, [> `Msg of string ]) result
(** List available collections in the calendar_dir. Returns Ok with the list of
    collection names if successful, or Error with a message if the directory
    cannot be read. *)

val get_collection :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  calendar_dir ->
  collection ->
  (calendar_file list, [> `Msg of string | `Not_found ]) result
(** Get all calendar files in a collection. If the collection doesn't exist in
    the cache, it will be loaded from disk. *)

val get_collections :
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  calendar_dir ->
  ((collection * calendar_file list) list, [> `Msg of string ]) result
(** Get all collections with their calendar files. This will load any
    collections that haven't been loaded yet. *)
