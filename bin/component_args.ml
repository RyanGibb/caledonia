open Cmdliner

let component_type_arg =
  let doc = "Type of component to add (event, todo, journal)" in
  let comp_type_enum = ["event"; "todo"; "journal"] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "event"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let categories_arg =
  let doc = "Comma-separated list of categories" in
  Arg.(
    value
    & opt (some string) None
    & info [ "categories"; "C" ] ~docv:"CATEGORIES" ~doc)

let priority_arg =
  let doc = "Priority level (1-9, 1 is highest)" in
  Arg.(
    value
    & opt (some int) None
    & info [ "priority"; "p" ] ~docv:"PRIORITY" ~doc)

let due_date_arg =
  let doc = "Due date for todo (YYYY-MM-DD)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "due"] ~docv:"DUE_DATE" ~doc)

let due_time_arg =
  let doc = "Due time for todo (HH:MM)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "due-time"] ~docv:"DUE_TIME" ~doc)

let percent_arg =
  let doc = "Percent complete (0-100)" in
  Arg.(
    value
    & opt (some int) None
    & info [ "percent"] ~docv:"PERCENT" ~doc)

let status_arg =
  let doc = "Status (draft, final, cancelled, needs-action, completed, in-process, tentative, confirmed)" in
  let status_enum = [
    ("draft", `Draft);
    ("final", `Final);
    ("cancelled", `Cancelled);
    ("needs-action", `Needs_action);
    ("completed", `Completed);
    ("in-process", `In_process);
    ("tentative", `Tentative);
    ("confirmed", `Confirmed);
  ] in
  Arg.(
    value
    & opt (some (enum status_enum)) None
    & info [ "status"] ~docv:"STATUS" ~doc)

let parent_arg =
  let doc = "Parent todo UID (for subtasks)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "parent" ] ~docv:"PARENT_UID" ~doc)

let no_parent_flag =
  let doc = "Remove parent relationship" in
  Arg.(
    value
    & flag
    & info [ "no-parent" ] ~doc)

let no_alarms_flag =
  let doc = "Remove all alarms" in
  Arg.(
    value
    & flag
    & info [ "no-alarms" ] ~doc)