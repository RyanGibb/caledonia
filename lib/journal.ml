open Icalendar

type t = {
  calendar_name : string;
  file : Eio.Fs.dir_ty Eio.Path.t;
  props : journal_prop list;
  calendar : calendar;
}

let get_id t =
  List.find_map
    (function `Uid (_, id) -> Some id | _ -> None)
    t.props
  |> Option.value ~default:""

let sexp_of_t t =
  Sexplib.Sexp.List [
    Sexplib.Sexp.Atom "journal";
    Sexplib.Sexp.Atom (get_id t);
    Sexplib.Sexp.Atom t.calendar_name;
  ]

let generate_uuid () =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let default_prodid = `Prodid (Params.empty, "-//Freumh//Caledonia//EN")

let create ~fs ~calendar_dir_path ?summary ?start ?description ?categories
    ?status calendar_name =
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
  let calendar = ([ default_prodid ], [ `Journal props ]) in
  Ok { calendar_name; file; props; calendar }

let edit ?summary ?start ?description ?categories ?status t =
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
    |> add_if_missing (function `Description _ -> true | _ -> false)
         (fun d -> `Description (Params.empty, d)) description
    |> add_if_missing (function `Categories _ -> true | _ -> false)
         (fun cats -> `Categories (Params.empty, cats)) categories
    |> add_if_missing (function `Status _ -> true | _ -> false)
         (fun s -> `Status (Params.empty, s)) status
  in
  let calendar = (fst t.calendar, [ `Journal props ]) in
  Ok { t with props; calendar }

let journals_of_icalendar calendar_name ~file calendar =
  let journals =
    List.filter_map
      (function `Journal props -> Some props | _ -> None)
      (snd calendar)
  in
  List.map
    (fun props -> { calendar_name; file; props; calendar = (fst calendar, [ `Journal props ]) })
    journals

let to_ical_journal t = t.props
let to_ical_calendar t = t.calendar

let get_summary t =
  List.find_map
    (function `Summary (_, s) -> Some s | _ -> None)
    t.props

let get_start t =
  match List.find_map
    (function
      | `Dtstart (_, `Date date) ->
          let (y, m, d) = date in
          Ptime.of_date_time ((y, m, d), ((0, 0, 0), 0))
      | `Dtstart (_, `Datetime ts) -> (
          match ts with
          | `Utc t | `Local t | `With_tzid (t, _) -> Some t)
      | _ -> None)
    t.props
  with
  | Some start -> Some start
  | None ->
      List.find_map
        (function `Dtstamp (_, t) -> Some t | _ -> None)
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

let get_calendar_name t = t.calendar_name
let get_file t = t.file

type format = [ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ]

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

let text_journal_data ?tz journal =
  let id = get_id journal in
  let calendar_name = get_calendar_name journal in
  let summary =
    match get_summary journal with
    | Some s when s <> "" -> s
    | _ ->
        (match get_description journal with
        | Some desc ->
            (match String.split_on_char '\n' desc with
            | first :: _ when first <> "" -> first
            | _ -> "")
        | None -> "")
  in
  let date_str =
    match get_start journal with
    | Some start -> Format_utils.format_date ?tz start
    | None -> ""
  in
  let categories = get_categories journal in
  let cats_str = if categories = [] then "" else String.concat "," categories in
  (calendar_name, date_str, summary, cats_str, id)

let format_journal ?(format = `Text) ?tz journal =
  match format with
  | `Text ->
      let calendar_name, date, summary, cats, id =
        text_journal_data ?tz journal
      in
      Printf.sprintf "%s\t%s\t%s\t%s\t%s" calendar_name date summary cats id
  | `Entries ->
      let summary_str = Format_utils.format_opt "Summary" Fun.id (get_summary journal) in
      let start_str = Format_utils.format_opt "Date" (Format_utils.format_date ?tz) (get_start journal) in
      let cats = get_categories journal in
      let cats_str = if cats = [] then "" else Printf.sprintf "Categories: %s\n" (String.concat ", " cats) in
      let description_str = Format_utils.format_opt "Description" Fun.id (get_description journal) in
      let status_str = Format_utils.format_opt "Status" (function
        | `Draft -> "Draft"
        | `Final -> "Final"
        | `Cancelled -> "Cancelled"
        | _ -> "Unknown") (get_status journal) in
      let other_props_str =
        List.filter_map format_prop_value journal.props
        |> List.map (fun (name, value) -> Printf.sprintf "%s: %s\n" name value)
        |> String.concat ""
      in
      let file_str = Format_utils.format_opt "File" Fun.id (Some (snd (get_file journal))) in
      Printf.sprintf "%s%s%s%s%s%s%s" summary_str start_str cats_str description_str status_str other_props_str file_str
  | `Json ->
      let open Yojson.Safe in
      let json =
        `Assoc
          [
            ("id", `String (get_id journal));
            ("summary", match get_summary journal with Some s -> `String s | None -> `Null);
            ("start", match get_start journal with Some s -> `String (Ptime.to_rfc3339 s) | None -> `Null);
            ("categories", `List (List.map (fun c -> `String c) (get_categories journal)));
            ("description", match get_description journal with Some d -> `String d | None -> `Null);
            ("status", match get_status journal with
              | Some `Draft -> `String "draft"
              | Some `Final -> `String "final"
              | Some `Cancelled -> `String "cancelled"
              | _ -> `Null);
            ("calendar", `String (get_calendar_name journal));
          ]
      in
      to_string json
  | `Csv ->
      let summary = Option.value ~default:"" (get_summary journal) in
      let date = match get_start journal with Some d -> Format_utils.format_date ?tz d | None -> "" in
      let cats = String.concat "," (get_categories journal) in
      let cal_id = get_calendar_name journal in
      Printf.sprintf "\"%s\",\"%s\",\"%s\",\"%s\"" summary date cats cal_id
  | `Ics ->
      let calendar = to_ical_calendar journal in
      Icalendar.to_ics ~cr:true calendar
  | `Sexp ->
      let summary = Option.value ~default:"" (get_summary journal) in
      let start_str = match get_start journal with
        | Some s -> Printf.sprintf "\"%s\"" (Ptime.to_rfc3339 s)
        | None -> "nil"
      in
      let cats = get_categories journal in
      let cats_str = if cats = [] then "nil"
        else Printf.sprintf "(%s)" (String.concat " " (List.map (fun c -> Printf.sprintf "\"%s\"" (String.escaped c)) cats)) in
      let calendar = Printf.sprintf "\"%s\"" (String.escaped (get_calendar_name journal)) in
      let id = get_id journal in
      Printf.sprintf
        "((:id \"%s\" :summary \"%s\" :start %s :categories %s :calendar %s))"
        (String.escaped id) (String.escaped summary) start_str cats_str calendar

let format_journals_with_dynamic_columns ?tz journals =
  if journals = [] then ""
  else
    let journal_data = List.map (text_journal_data ?tz) journals in
    let max_cal_width = Format_utils.max_width (fun (cal, _, _, _, _) -> cal) journal_data in
    let max_date_width = Format_utils.max_width (fun (_, date, _, _, _) -> date) journal_data in
    let max_summary_width = Format_utils.max_width (fun (_, _, summary, _, _) -> summary) journal_data in
    let max_cats_width = Format_utils.max_width (fun (_, _, _, cats, _) -> cats) journal_data in
    let max_id_width = Format_utils.max_width (fun (_, _, _, _, id) -> id) journal_data in
    List.map
      (fun (cal, date, summary, cats, id) ->
        Printf.sprintf "%s  %s  %s  %s  %s"
          (Format_utils.pad_to_width max_cal_width cal)
          (Format_utils.pad_to_width max_date_width date)
          (Format_utils.pad_to_width max_summary_width summary)
          (Format_utils.pad_to_width max_cats_width cats)
          (Format_utils.pad_to_width max_id_width id))
      journal_data
    |> String.concat "\n"

let format_journals ?(format = `Text) ?tz journals =
  match format with
  | `Text -> format_journals_with_dynamic_columns ?tz journals
  | `Json ->
      let json_journals =
        List.map
          (fun j -> Yojson.Safe.from_string (format_journal ~format:`Json ?tz j))
          journals
      in
      Yojson.Safe.to_string (`List json_journals)
  | `Sexp ->
      "("
      ^ String.concat "\n "
          (List.map (fun j -> format_journal ~format:`Sexp ?tz j) journals)
      ^ ")"
  | _ -> String.concat "\n" (List.map (fun j -> format_journal ~format ?tz j) journals)
