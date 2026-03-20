# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Caledonia is an OCaml calendar client with CLI and Emacs front-ends. It operates on vdir directories of `.ics` files (RFC 5545), enabling interaction with CalDAV servers via tools like vdirsyncer.

## Build Commands

```bash
dune build              # Build the project
dune runtest            # Run the full test suite (inline expect tests via ppx_expect)
dune exec -- caled      # Run the CLI tool
opam install .          # Install via opam
```

There is no separate lint command; `dune build` catches type errors and warnings. Code formatting uses ocamlformat with default settings (`.ocamlformat` is empty).

## Architecture

### Two-layer structure

- **`lib/`** — Core library (`caledonia_lib`). Pure logic for calendar operations:
  - `event.ml` — Event type, recurrence expansion (RRULE), querying/filtering, multi-format output (text, JSON, CSV, ICS, S-expressions)
  - `date.ml` — Date parsing, timezone handling (via timedesc/timere), week/month expressions
  - `todo.ml`, `journal.ml` — TODO and Journal component types
  - `component.ml` — Generic component abstraction over events/todos/journals
  - `calendar_dir.ml` — Reads vdir directory structure, manages `.ics` files on disk
  - `sexp.ml` — S-expression serialization for Emacs protocol
  - `format_utils.ml` — Shared formatting (column alignment, Unicode-aware width)

- **`bin/`** — CLI executable (`caled`), built with Cmdliner:
  - `main.ml` — Entry point, subcommand registration, `CALENDAR_DIR` env var handling
  - `*_cmd.ml` — One module per subcommand (list, search, show, add, delete, edit, server)
  - `query_args.ml`, `event_args.ml`, `component_args.ml` — Shared CLI argument definitions

- **`emacs/`** — Emacs front-end communicating with `caled server` via S-expression protocol

### Key patterns

- Uses **Eio** for structured concurrency and filesystem access (not Lwt/Async)
- PPX preprocessors: `ppx_deriving.show`, `ppx_deriving.eq`, `ppx_sexp_conv` for code generation
- Tests use **ppx_expect** inline expect tests (not alcotest), run via `dune runtest`
- Test data lives in `test/calendar/` with `example/` and `recurrence/` subdirectories containing `.ics` fixtures

### Configuration

The tool reads calendars from `CALENDAR_DIR` environment variable, defaulting to `~/.calendar/`. Each subdirectory is treated as a named calendar containing `.ics` files.
