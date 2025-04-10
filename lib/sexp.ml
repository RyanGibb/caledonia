open Sexplib.Std

type search_field = Summary | Description | Location [@@deriving sexp]

type query_request = {
  from : string option; [@sexp.option]
  to_ : string; (* Required field, not optional *)
  timezone : string option; [@sexp.option]
  calendars : string list; [@default []]
  text : string option; [@sexp.option]
  search_in : search_field list; [@default []]
  id : string option; [@sexp.option]
  recurring : bool option; [@sexp.option]
  limit : int option; [@sexp.option]
}
[@@deriving sexp]

(* workaround https://github.com/janestreet/ppx_sexp_conv/issues/18#issuecomment-2792574295 *)
let query_request_of_sexp sexp =
  let open Sexplib.Sexp in
  let sexp = match sexp with
    | List ss ->
      List (List.map (function List (Atom "to" :: v) -> List (Atom "to_" :: v) | v -> v) ss)
    | v -> v
  in
  query_request_of_sexp sexp

let sexp_of_query_request q =
  let open Sexplib.Sexp in
  let sexp = sexp_of_query_request q in
  let sexp = match sexp with
    | List ss ->
      List (List.map (function List (Atom "to_" :: v) -> List (Atom "to" :: v) | v -> v) ss)
    | v -> v
  in
  sexp

type request = ListCalendars | Query of query_request | Refresh [@@deriving sexp]

type response_payload = Calendars of string list | Events of Event.t list | Empty
[@@deriving sexp_of]

type response = Ok of response_payload | Error of string [@@deriving sexp_of]

let filter_func_of_search_field text = function
  | Summary -> Event.summary_contains text
  | Description -> Event.description_contains text
  | Location -> Event.location_contains text

let parse_timezone ~timezone =
  match timezone with
  | Some tzid -> (
      match Timedesc.Time_zone.make tzid with
      | Some tz -> tz
      | None -> failwith ("Invalid timezone: " ^ tzid))
  | None -> !Date.default_timezone ()

let generate_query_params (req : query_request) =
  let ( let* ) = Result.bind in
  let tz = parse_timezone ~timezone:req.timezone in
  let* from =
    match req.from with
    | None -> Ok None
    | Some s -> Result.map Option.some (Date.parse_date ~tz s `From)
  in
  let* to_ =
    let* to_date = Date.parse_date ~tz req.to_ `To in
    Ok (Date.to_end_of_day to_date)
  in
  let filters = ref [] in
  (match req.calendars with
  | [] -> ()
  | cals -> filters := Event.in_calendars cals :: !filters);
  (match req.text with
  | Some text ->
      let search_fields =
        match req.search_in with
        | [] -> [ Summary; Description; Location ]
        | fields -> fields
      in
      let text_filters =
        List.map (filter_func_of_search_field text) search_fields
      in
      filters := Event.or_filter text_filters :: !filters
  | None -> ());
  (match req.id with
  | Some id -> filters := Event.with_id id :: !filters
  | None -> ());
  (match req.recurring with
  | Some true -> filters := Event.recurring_only () :: !filters
  | Some false -> filters := Event.non_recurring_only () :: !filters
  | _ -> ());
  let final_filter = Event.and_filter !filters in
  let limit = req.limit in
  Ok (final_filter, from, to_, limit, tz)

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let needs_quotes s =
  String.exists
    (fun c -> is_whitespace c || c = '(' || c = ')' || c = '"' || c = '\'')
    s

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  "\"" ^ Buffer.contents buf ^ "\""

let rec to_string = function
  | Sexplib.Sexp.Atom str -> if needs_quotes str then escape str else str
  | Sexplib.Sexp.List lst ->
      "(" ^ String.concat " " (List.map to_string lst) ^ ")"
