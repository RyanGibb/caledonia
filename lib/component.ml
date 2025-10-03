type component_type = CEvent | CTodo | CJournal [@@deriving sexp]

type t =
  | Event of Event.t
  | Todo of Todo.t
  | Journal of Journal.t

let sexp_of_t = function
  | Event e -> Event.sexp_of_t e
  | Todo t -> Todo.sexp_of_t t
  | Journal j -> Journal.sexp_of_t j

let component_type = function
  | Event _ -> CEvent
  | Todo _ -> CTodo
  | Journal _ -> CJournal

let of_event e = Event e
let of_todo t = Todo t
let of_journal j = Journal j

let to_event = function Event e -> Some e | _ -> None
let to_todo = function Todo t -> Some t | _ -> None
let to_journal = function Journal j -> Some j | _ -> None

let get_id = function
  | Event e -> Event.get_id e
  | Todo t -> Todo.get_id t
  | Journal j -> Journal.get_id j

let get_summary = function
  | Event e -> Event.get_summary e
  | Todo t -> Todo.get_summary t
  | Journal j -> Journal.get_summary j

let get_description = function
  | Event e -> Event.get_description e
  | Todo t -> Todo.get_description t
  | Journal j -> Journal.get_description j

let get_categories = function
  | Event e -> Event.get_categories e
  | Todo t -> Todo.get_categories t
  | Journal j -> Journal.get_categories j

let get_calendar_name = function
  | Event e -> Event.get_calendar_name e
  | Todo t -> Todo.get_calendar_name t
  | Journal j -> Journal.get_calendar_name j

let get_file = function
  | Event e -> Event.get_file e
  | Todo t -> Todo.get_file t
  | Journal j -> Journal.get_file j

let get_start = function
  | Event e -> Some (Event.get_start e)
  | Todo t -> Todo.get_start t
  | Journal j -> Journal.get_start j

let to_ical_component = function
  | Event e -> `Event (Event.to_ical_event e)
  | Todo t -> `Todo (Todo.to_ical_todo t, [])
  | Journal j -> `Journal (Journal.to_ical_journal j)

let to_ical_calendar = function
  | Event e -> Event.to_ical_calendar e
  | Todo t -> Todo.to_ical_calendar t
  | Journal j -> Journal.to_ical_calendar j

let components_of_icalendar calendar_name ~file calendar =
  let events =
    Event.events_of_icalendar calendar_name ~file calendar
    |> List.map of_event
  in
  let todos =
    Todo.todos_of_icalendar calendar_name ~file calendar
    |> List.map of_todo
  in
  let journals =
    Journal.journals_of_icalendar calendar_name ~file calendar
    |> List.map of_journal
  in
  events @ todos @ journals

(* Comparators *)
type comparator = t -> t -> int

let by_start a b =
  match (get_start a, get_start b) with
  | None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some ta, Some tb -> Ptime.compare ta tb

let by_summary a b =
  match (get_summary a, get_summary b) with
  | None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some sa, Some sb -> String.compare sa sb

let by_calendar_name a b =
  String.compare (get_calendar_name a) (get_calendar_name b)

let by_type a b =
  match (component_type a, component_type b) with
  | CEvent, CEvent | CTodo, CTodo | CJournal, CJournal -> 0
  | CEvent, _ -> -1
  | _, CEvent -> 1
  | CTodo, CJournal -> -1
  | CJournal, CTodo -> 1

let descending comp a b = -comp a b
let chain comp1 comp2 a b =
  match comp1 a b with 0 -> comp2 a b | n -> n

(* Filters *)
type filter = t -> bool

let is_type typ comp = component_type comp = typ

let summary_contains pattern comp =
  match get_summary comp with
  | Some s -> Re.execp (Re.compile (Re.str pattern |> Re.no_case)) s
  | None -> false

let description_contains pattern comp =
  match get_description comp with
  | Some d -> Re.execp (Re.compile (Re.str pattern |> Re.no_case)) d
  | None -> false

let in_calendars calendars comp =
  List.mem (get_calendar_name comp) calendars

let has_categories cats comp =
  let comp_cats = get_categories comp in
  List.exists (fun cat -> List.mem cat comp_cats) cats

let and_filter filters comp =
  List.for_all (fun f -> f comp) filters

let or_filter filters comp =
  List.exists (fun f -> f comp) filters

let not_filter f comp = not (f comp)

(* Formatting *)
type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

let format_component ?(format = `Text) ?(tz = !Date.default_timezone) comp =
  let tz_val = tz () in
  match comp with
  | Event e -> Event.format_event ~format ~tz:tz_val e
  | Todo t -> Todo.format_todo ~format ~tz:tz_val t
  | Journal j -> Journal.format_journal ~format ~tz:tz_val j

let format_components ?(format = `Text) ?(tz = !Date.default_timezone) comps =
  let events = List.filter_map to_event comps in
  let todos = List.filter_map to_todo comps in
  let journals = List.filter_map to_journal comps in
  let tz_val = tz () in
  let event_str = Event.format_events ~format ~tz:tz_val events in
  let todo_str = Todo.format_todos ~format ~tz:tz_val todos in
  let journal_str = Journal.format_journals ~format ~tz:tz_val journals in
  match format with
  | `Json ->
      Printf.sprintf "{\"events\":%s,\"todos\":%s,\"journals\":%s}" event_str todo_str journal_str
  | `Sexp ->
      Printf.sprintf "(:events %s :todos %s :journals %s)" event_str todo_str journal_str
  | _ ->
      let parts = [event_str; todo_str; journal_str] in
      let non_empty = List.filter (fun s -> s <> "") parts in
      String.concat "\n" non_empty