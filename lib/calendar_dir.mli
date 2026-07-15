(** Functions for managing calendar directories of .ics files *)

type t
(** A vdir-style calendar directory, where each calendar is a subdirectory
    containing .ics files *)

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t -> string -> (t, [> `Msg of string ]) result
(** Create a calendar_dir from a directory path. Returns Ok with the
    calendar_dir if successful, or Error with a message if the directory cannot
    be created or accessed. *)

val get_display_name :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> string -> string
(** Get the display name for a calendar from its displayname file, falling back
    to the directory name if the file doesn't exist. *)

val get_color :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> string -> string option
(** Get the color for a calendar from its color file. Returns None if the file
    doesn't exist or is empty. *)

val find_calendar_by_display_name :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> string -> string option
(** Find the calendar directory name by its display name. Returns the directory
    name if found, or None if no calendar matches the display name. Also accepts
    the actual directory name as input. *)

val list_calendar_names :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (string list, [> `Msg of string ]) result
(** List available calendars in the calendar_dir. Returns Ok with the list of
    calendar names if successful, or Error with a message if the directory
    cannot be read. *)

(** Component operations *)

val get_calendar_components :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (Component.t list, [> `Msg of string | `Not_found ]) result
(** Get all components in a calendar. *)

val get_components :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (Component.t list, [> `Msg of string ]) result
(** Get all components in all calendars. *)

val add_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t list ->
  Component.t ->
  (Component.t list, [> `Msg of string ]) result
(** Add a component to the calendar directory. *)

val edit_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t list ->
  Component.t ->
  (Component.t list, [> `Msg of string ]) result
(** Edit a component in the calendar directory. *)

val delete_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t list ->
  Component.t ->
  (Component.t list, [> `Msg of string ]) result
(** Delete a component from the calendar directory. *)

(** Event-typed convenience wrappers over the component operations, used by
    the event subcommands, the server, and the tests. *)

val get_calendar_events :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (Event.t list, [> `Msg of string | `Not_found ]) result
(** Get all event components in a calendar. *)

val get_events :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (Event.t list, [> `Msg of string ]) result
(** Get all events in all calendars. *)

val add_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t list ->
  Event.t ->
  (Event.t list, [> `Msg of string ]) result
(** Add an event to the calendar directory. *)

val edit_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t list ->
  Event.t ->
  (Event.t list, [> `Msg of string ]) result
(** Edit an event in the calendar directory. *)

val delete_event :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t list ->
  Event.t ->
  (Event.t list, [> `Msg of string ]) result
(** Delete an event from the calendar directory. *)

val delete_occurrence :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t list ->
  Event.t ->
  Ptime.t ->
  (Event.t list, [> `Msg of string ]) result
(** Delete a single occurrence of a recurring event by adding EXDATE. *)

val add_occurrence_override :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Event.t list ->
  Event.t ->
  Icalendar.event ->
  (Event.t list, [> `Msg of string ]) result
(** Add a RECURRENCE-ID override VEVENT to an existing recurring event's .ics file. *)

val get_path : t -> string
