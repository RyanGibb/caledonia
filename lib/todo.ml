open Icalendar

type t = {
  calendar_name : string;
  file : Eio.Fs.dir_ty Eio.Path.t;
  props : todo_prop list;
  calendar : calendar;
}

let get_id t =
  List.find_map
    (function `Uid (_, id) -> Some id | _ -> None)
    t.props
  |> Option.value ~default:""

let sexp_of_t t =
  Sexplib.Sexp.List [
    Sexplib.Sexp.Atom "todo";
    Sexplib.Sexp.Atom (get_id t);
    Sexplib.Sexp.Atom t.calendar_name;
  ]

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")

let create ~fs ~calendar_dir_path ?summary ?start ?due ?description ?categories
    ?status ?priority ?percent ?parent calendar_name =
  let uuid = generate_uuid () in
  let uid = (Params.empty, uuid) in
  let file_name = uuid ^ ".ics" in
  let file = Eio.Path.(fs / calendar_dir_path / calendar_name / file_name) in
  let now = Ptime_clock.now () in
  let props = [ `Dtstamp (Params.empty, now); `Uid uid ] in
  let props =
    match summary with
    | Some s -> `Summary (Params.empty, s) :: props
    | None -> props
  in
  let props =
    match start with Some s -> `Dtstart s :: props | None -> props
  in
  let props =
    match due with Some d -> `Due d :: props | None -> props
  in
  let props =
    match description with
    | Some d -> `Description (Params.empty, d) :: props
    | None -> props
  in
  let props =
    match categories with
    | Some cats -> `Categories (Params.empty, cats) :: props
    | None -> props
  in
  let props =
    match status with Some s -> `Status (Params.empty, s) :: props | None -> props
  in
  let props =
    match priority with
    | Some p when p >= 0 && p <= 9 -> `Priority (Params.empty, p) :: props
    | _ -> props
  in
  let props =
    match percent with
    | Some p when p >= 0 && p <= 100 -> `Percent (Params.empty, p) :: props
    | _ -> props
  in
  let props =
    match parent with
    | Some parent_uid ->
        let params = Params.empty |> Params.add Reltype `Parent in
        `Related (params, parent_uid) :: props
    | None -> props
  in
  let calendar = ([ default_prodid ], [ `Todo (props, []) ]) in
  Ok { calendar_name; file; props; calendar }

let edit ?summary ?start ?due ?description ?categories ?status ?priority ?percent ?parent t =
  let now = Ptime_clock.now () in
  let props =
    List.filter_map
      (function
        | `Uid _ as uid -> Some uid
        | `Dtstamp _ -> Some (`Dtstamp (Params.empty, now))
        | `Summary _ as prop -> (
            match summary with
            | Some s -> Some (`Summary (Params.empty, s))
            | None -> Some prop)
        | `Dtstart _ as prop -> (
            match start with Some s -> Some (`Dtstart s) | None -> Some prop)
        | `Due _ as prop -> (
            match due with Some d -> Some (`Due d) | None -> Some prop)
        | `Description _ as prop -> (
            match description with
            | Some d -> Some (`Description (Params.empty, d))
            | None -> Some prop)
        | `Categories _ as prop -> (
            match categories with
            | Some cats -> Some (`Categories (Params.empty, cats))
            | None -> Some prop)
        | `Status _ as prop -> (
            match status with
            | Some s -> Some (`Status (Params.empty, s))
            | None -> Some prop)
        | `Priority _ as prop -> (
            match priority with
            | Some p when p >= 0 && p <= 9 -> Some (`Priority (Params.empty, p))
            | None -> Some prop
            | _ -> None)
        | `Percent _ as prop -> (
            match percent with
            | Some p when p >= 0 && p <= 100 -> Some (`Percent (Params.empty, p))
            | None -> Some prop
            | _ -> None)
        | (`Related (params, _) | `Iana_prop ("RELATED", params, _)) as prop -> (
            match parent with
            | Some None -> None
            | Some (Some _) -> (
                match Icalendar.Params.find Reltype params with
                | Some `Parent | None -> None
                | _ -> Some prop)
            | None -> Some prop)
        | prop -> Some prop)
      t.props
  in
  let props =
    let add_if_missing pred make_prop value props =
      if value <> None && not (List.exists pred props)
      then make_prop (Option.get value) :: props
      else props
    in
    props
    |> add_if_missing (function `Summary _ -> true | _ -> false)
         (fun s -> `Summary (Params.empty, s)) summary
    |> add_if_missing (function `Dtstart _ -> true | _ -> false)
         (fun s -> `Dtstart s) start
    |> add_if_missing (function `Due _ -> true | _ -> false)
         (fun d -> `Due d) due
    |> add_if_missing (function `Description _ -> true | _ -> false)
         (fun d -> `Description (Params.empty, d)) description
    |> add_if_missing (function `Categories _ -> true | _ -> false)
         (fun cats -> `Categories (Params.empty, cats)) categories
    |> add_if_missing (function `Status _ -> true | _ -> false)
         (fun s -> `Status (Params.empty, s)) status
    |> add_if_missing (function `Priority _ -> true | _ -> false)
         (fun p -> `Priority (Params.empty, p)) priority
    |> add_if_missing (function `Percent _ -> true | _ -> false)
         (fun p -> `Percent (Params.empty, p)) percent
  in
  let props =
    match parent with
    | Some (Some parent_uid) ->
        let params = Icalendar.Params.empty |> Icalendar.Params.add Reltype `Parent in
        `Related (params, parent_uid) :: props
    | _ -> props
  in
  let calendar = (fst t.calendar, [ `Todo (props, []) ]) in
  Ok { t with props; calendar }

let mark_complete t =
  let now = Ptime_clock.now () in
  let props =
    List.filter
      (function `Percent _ | `Status _ | `Completed _ -> false | _ -> true)
      t.props
  in
  let props =
    `Completed (Params.empty, now)
    :: `Status (Params.empty, `Completed)
    :: `Percent (Params.empty, 100)
    :: props
  in
  let calendar = (fst t.calendar, [ `Todo (props, []) ]) in
  Ok { t with props; calendar }

let set_percent percent t =
  if percent < 0 || percent > 100 then
    Error (`Msg "Percent must be between 0 and 100")
  else
    let props =
      List.filter (function `Percent _ -> false | _ -> true) t.props
    in
    let props = `Percent (Params.empty, percent) :: props in
    let props =
      if percent = 100 then
        let props = List.filter (function `Status _ | `Completed _ -> false | _ -> true) props in
        `Completed (Params.empty, Ptime_clock.now ())
        :: `Status (Params.empty, `Completed)
        :: props
      else props
    in
    let calendar = (fst t.calendar, [ `Todo (props, []) ]) in
    Ok { t with props; calendar }

let todos_of_icalendar calendar_name ~file calendar =
  let todos =
    List.filter_map
      (function `Todo (props, _) -> Some props | _ -> None)
      (snd calendar)
  in
  List.map
    (fun props -> { calendar_name; file; props; calendar = (fst calendar, [ `Todo (props, []) ]) })
    todos

let to_ical_todo t = t.props
let to_ical_calendar t = t.calendar


let get_summary t =
  List.find_map
    (function `Summary (_, s) -> Some s | _ -> None)
    t.props

let get_start t =
  List.find_map
    (function
      | `Dtstart (_, `Date date) ->
          let (y, m, d) = date in
          Ptime.of_date_time ((y, m, d), ((0, 0, 0), 0))
      | `Dtstart (_, `Datetime ts) -> (
          match ts with
          | `Utc t | `Local t | `With_tzid (t, _) -> Some t)
      | _ -> None)
    t.props

let get_due t =
  List.find_map
    (function
      | `Due (_, `Date date) ->
          let (y, m, d) = date in
          Ptime.of_date_time ((y, m, d), ((0, 0, 0), 0))
      | `Due (_, `Datetime ts) -> (
          match ts with
          | `Utc t | `Local t | `With_tzid (t, _) -> Some t)
      | _ -> None)
    t.props

let get_description t =
  List.find_map
    (function `Description (_, d) -> Some d | _ -> None)
    t.props

let get_categories t =
  List.find_map
    (function `Categories (_, cats) -> Some cats | _ -> None)
    t.props
  |> Option.value ~default:[]

let get_status t =
  List.find_map
    (function `Status (_, s) -> Some s | _ -> None)
    t.props

let get_priority t =
  List.find_map
    (function `Priority (_, p) -> Some p | _ -> None)
    t.props

let get_percent t =
  List.find_map
    (function `Percent (_, p) -> Some p | _ -> None)
    t.props

let get_completed t =
  List.find_map
    (function `Completed (_, t) -> Some t | _ -> None)
    t.props

let get_calendar_name t = t.calendar_name
let get_file t = t.file

let get_related_parent t =
  List.find_map
    (function
      | `Related (params, uid) -> (
          match Icalendar.Params.find Reltype params with
          | Some `Parent | None -> Some uid
          | _ -> None)
      | `Iana_prop ("RELATED", params, uid) -> (
          match Icalendar.Params.find Reltype params with
          | Some `Parent | None -> Some uid
          | _ -> None)
      | _ -> None)
    t.props

let is_completed t =
  match get_status t with Some `Completed -> true | _ -> false

let is_overdue t =
  match (get_due t, is_completed t) with
  | Some due, false -> Ptime.compare due (Ptime_clock.now ()) < 0
  | _ -> false

type todo_tree = {
  todo : t;
  children : todo_tree list;
}

let rec get_ancestors ~all_todos todo =
  match get_related_parent todo with
  | None -> []
  | Some parent_id ->
      (match List.find_opt (fun t -> get_id t = parent_id) all_todos with
      | Some parent -> parent :: get_ancestors ~all_todos parent
      | None -> [])

let expand_with_ancestors ~all_todos ~filtered_todos =
  let ancestors =
    List.concat_map (fun todo -> get_ancestors ~all_todos todo) filtered_todos
  in
  let all_ids = Hashtbl.create 100 in
  List.iter (fun todo -> Hashtbl.replace all_ids (get_id todo) todo) filtered_todos;
  List.iter (fun todo -> Hashtbl.replace all_ids (get_id todo) todo) ancestors;
  Hashtbl.fold (fun _ todo acc -> todo :: acc) all_ids []

let build_todo_tree todos =
  let todo_map =
    List.fold_left
      (fun acc todo -> (get_id todo, todo) :: acc)
      [] todos
    |> List.to_seq
    |> Hashtbl.of_seq
  in
  let rec build_tree visited todo =
    let id = get_id todo in
    if List.mem id visited then
      { todo; children = [] }
    else
      let visited = id :: visited in
      let children =
        List.filter_map
          (fun t ->
            match get_related_parent t with
            | Some parent_id when parent_id = id ->
                Some (build_tree visited t)
            | _ -> None)
          todos
      in
      { todo; children }
  in
  List.filter_map
    (fun todo ->
      match get_related_parent todo with
      | None -> Some (build_tree [] todo)
      | Some parent_id ->
          if Hashtbl.mem todo_map parent_id then None
          else Some (build_tree [] todo))
    todos

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

let text_todo_data ?tz todo =
  let id = get_id todo in
  let calendar_name = get_calendar_name todo in
  let summary = Option.value ~default:"" (get_summary todo) in
  let status_str =
    if is_completed todo then "[x]"
    else if is_overdue todo then "[!]"
    else "[ ]"
  in
  let start_str = match get_start todo with
    | Some start -> Format_utils.format_date ?tz start
    | None -> ""
  in
  let due_str = match get_due todo with
    | Some due -> Format_utils.format_date ?tz due
    | None -> ""
  in
  let percent_str = match get_percent todo with
    | Some p -> Printf.sprintf "%d%%" p
    | None -> ""
  in
  let categories = get_categories todo in
  let cats_str = if categories = [] then "" else String.concat "," categories in
  (calendar_name, start_str, due_str, status_str, summary, percent_str, cats_str, id)

let format_prop_value = function
  | `Related (params, s) ->
      let reltype =
        match Icalendar.Params.find Reltype params with
        | Some `Parent -> "PARENT"
        | Some `Child -> "CHILD"
        | Some `Sibling -> "SIBLING"
        | Some (`Ianatoken t) -> t
        | Some (`Xname (ns, name)) -> ns ^ ":" ^ name
        | None -> "PARENT"
      in
      Some ("Related-To", s ^ " (" ^ reltype ^ ")")
  | `Seq (_, n) -> Some ("Sequence", string_of_int n)
  | `Created (_, t) -> Some ("Created", Ptime.to_rfc3339 t)
  | `Lastmod (_, t) -> Some ("Last-Modified", Ptime.to_rfc3339 t)
  | `Iana_prop ("RELATED", params, value) ->
      let reltype =
        match Icalendar.Params.find Reltype params with
        | Some `Parent -> "PARENT"
        | Some `Child -> "CHILD"
        | Some `Sibling -> "SIBLING"
        | Some (`Ianatoken t) -> t
        | Some (`Xname (ns, name)) -> ns ^ ":" ^ name
        | None -> "PARENT"
      in
      Some ("Related-To", value ^ " (" ^ reltype ^ ")")
  | `Iana_prop (name, _, value) -> Some (name, value)
  | `Xprop ((ns, name), _, value) -> Some (ns ^ ":" ^ name, value)
  | _ -> None

let format_todo ?(format = `Text) ?tz todo =
  match format with
  | `Text ->
      let calendar_name, start, due, status, summary, percent, cats, id =
        text_todo_data ?tz todo
      in
      Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
        calendar_name start due status summary percent cats id
  | `Entries ->
      let summary_str = Format_utils.format_opt "Summary" Fun.id (get_summary todo) in
      let due_str = Format_utils.format_opt "Due" (Format_utils.format_date ?tz) (get_due todo) in
      let start_str = Format_utils.format_opt "Start" (Format_utils.format_date ?tz) (get_start todo) in
      let priority_str = Format_utils.format_opt "Priority" string_of_int (get_priority todo) in
      let percent_str = Format_utils.format_opt "Percent" (fun p -> string_of_int p ^ "%") (get_percent todo) in
      let status_str = Format_utils.format_opt "Status" (function
        | `Needs_action -> "Needs Action"
        | `Completed -> "Completed"
        | `In_process -> "In Process"
        | `Cancelled -> "Cancelled"
        | _ -> "Unknown") (get_status todo) in
      let cats = get_categories todo in
      let cats_str = if cats = [] then "" else Printf.sprintf "Categories: %s\n" (String.concat ", " cats) in
      let description_str = Format_utils.format_opt "Description" Fun.id (get_description todo) in
      let other_props_str =
        List.filter_map format_prop_value todo.props
        |> List.map (fun (name, value) -> Printf.sprintf "%s: %s\n" name value)
        |> String.concat ""
      in
      let file_str = Format_utils.format_opt "File" Fun.id (Some (snd (get_file todo))) in
      Printf.sprintf "%s%s%s%s%s%s%s%s%s%s" summary_str start_str due_str priority_str
        percent_str status_str cats_str description_str other_props_str file_str
  | `Json ->
      let open Yojson.Safe in
      let json =
        `Assoc
          [
            ("id", `String (get_id todo));
            ("summary", match get_summary todo with Some s -> `String s | None -> `Null);
            ("due", match get_due todo with Some d -> `String (Ptime.to_rfc3339 d) | None -> `Null);
            ("start", match get_start todo with Some s -> `String (Ptime.to_rfc3339 s) | None -> `Null);
            ("priority", match get_priority todo with Some p -> `Int p | None -> `Null);
            ("percent", match get_percent todo with Some p -> `Int p | None -> `Null);
            ("status", match get_status todo with
              | Some `Completed -> `String "completed"
              | Some `In_process -> `String "in-process"
              | Some `Needs_action -> `String "needs-action"
              | Some `Cancelled -> `String "cancelled"
              | _ -> `Null);
            ("categories", `List (List.map (fun c -> `String c) (get_categories todo)));
            ("description", match get_description todo with Some d -> `String d | None -> `Null);
            ("calendar", `String (get_calendar_name todo));
          ]
      in
      to_string json
  | `Csv ->
      let summary = Option.value ~default:"" (get_summary todo) in
      let due = match get_due todo with Some d -> Format_utils.format_date ?tz d | None -> "" in
      let priority = match get_priority todo with Some p -> string_of_int p | None -> "" in
      let percent = match get_percent todo with Some p -> string_of_int p | None -> "" in
      let cal_id = get_calendar_name todo in
      Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" summary due priority percent cal_id
  | `Ics ->
      let calendar = to_ical_calendar todo in
      Icalendar.to_ics ~cr:true calendar
  | `Sexp ->
      let summary = Option.value ~default:"" (get_summary todo) in
      let due_str = match get_due todo with
        | Some d -> Printf.sprintf "\"%s\"" (Ptime.to_rfc3339 d)
        | None -> "nil"
      in
      let priority = match get_priority todo with Some p -> string_of_int p | None -> "nil" in
      let percent = match get_percent todo with Some p -> string_of_int p | None -> "nil" in
      let status_str = match get_status todo with
        | Some `Completed -> "\"completed\""
        | Some `In_process -> "\"in-process\""
        | Some `Needs_action -> "\"needs-action\""
        | Some `Cancelled -> "\"cancelled\""
        | _ -> "nil"
      in
      let calendar = Printf.sprintf "\"%s\"" (String.escaped (get_calendar_name todo)) in
      let id = get_id todo in
      Printf.sprintf
        "((:id \"%s\" :summary \"%s\" :due %s :priority %s :percent %s :status %s :calendar %s))"
        (String.escaped id) (String.escaped summary) due_str priority percent status_str calendar

let format_todos_with_dynamic_columns ?tz ?get_color todos =
  if todos = [] then ""
  else
    let trees = build_todo_tree todos in
    let rec collect_all_todos_with_depth depth tree =
      (tree.todo, depth) :: List.concat_map (collect_all_todos_with_depth (depth + 1)) tree.children
    in
    let all_todos_with_depth = List.concat_map (collect_all_todos_with_depth 0) trees in
    let todo_data = List.map (fun (todo, depth) ->
      let cal, start, due, status, summary, percent, cats, id = text_todo_data ?tz todo in
      let indent = String.make (depth * 2) ' ' in
      let status_with_indent = indent ^ status in
      (cal, start, due, status_with_indent, summary, percent, cats, id)
    ) all_todos_with_depth in
    let max_cal_width = Format_utils.max_width (fun (cal, _, _, _, _, _, _, _) -> cal) todo_data in
    let max_start_width = Format_utils.max_width (fun (_, start, _, _, _, _, _, _) -> start) todo_data in
    let max_due_width = Format_utils.max_width (fun (_, _, due, _, _, _, _, _) -> due) todo_data in
    let max_status_width = Format_utils.max_width (fun (_, _, _, status, _, _, _, _) -> status) todo_data in
    let max_summary_width = Format_utils.max_width (fun (_, _, _, _, summary, _, _, _) -> summary) todo_data in
    let max_percent_width = Format_utils.max_width (fun (_, _, _, _, _, pct, _, _) -> pct) todo_data in
    let max_cats_width = Format_utils.max_width (fun (_, _, _, _, _, _, cats, _) -> cats) todo_data in
    let max_id_width = Format_utils.max_width (fun (_, _, _, _, _, _, _, id) -> id) todo_data in
    let rec format_tree depth tree =
      let indent = String.make (depth * 2) ' ' in
      let cal, start, due, status, summary, percent, cats, id = text_todo_data ?tz tree.todo in
      let color = match get_color with Some f -> f cal | None -> None in
      let status_with_indent = indent ^ status in
      let line =
        Printf.sprintf "%s  %s  %s  %s  %s  %s  %s  %s"
          (Format_utils.pad_to_width ?color max_cal_width cal)
          (Format_utils.pad_to_width max_start_width start)
          (Format_utils.pad_to_width max_due_width due)
          (Format_utils.pad_to_width max_status_width status_with_indent)
          (Format_utils.pad_to_width max_summary_width summary)
          (Format_utils.pad_to_width max_percent_width percent)
          (Format_utils.pad_to_width max_cats_width cats)
          (Format_utils.pad_to_width max_id_width id)
      in
      let children_lines = List.concat_map (format_tree (depth + 1)) tree.children in
      line :: children_lines
    in
    List.concat_map (format_tree 0) trees
    |> String.concat "\n"

let format_todos ?(format = `Text) ?tz ?get_color todos =
  match format with
  | `Text -> format_todos_with_dynamic_columns ?tz ?get_color todos
  | `Json ->
      let json_todos =
        List.map
          (fun t -> Yojson.Safe.from_string (format_todo ~format:`Json ?tz t))
          todos
      in
      Yojson.Safe.to_string (`List json_todos)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun t -> format_todo ~format:`Sexp ?tz t) todos)
      ^ ")"
  | _ -> String.concat "\n" (List.map (fun t -> format_todo ~format ?tz t) todos)
