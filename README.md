# 📅 Caledonia 🏴󠁧󠁢󠁳󠁣󠁴󠁿

Caledonia is a command-line calendar client.
Currently, it operates on directories of [`.ics`](https://datatracker.ietf.org/doc/html/rfc5545) files (as managed by tools like [vdirsyncer](https://github.com/pimutils/vdirsyncer)).
It has the `list`, `search`, `show`, `add`, `delete`, and `edit` subcommands, and supports timezones.
See [TODO](./TODO.org) for future plans.

## Configuration

Caledonia looks for calendars in the directory specified by the `CALENDAR_DIR` environment variable or in `~/.calendars/` by default.

## Tests

The project includes a test suite that can be ran with `dune runtest`.

## Thanks

To [Patrick](https://patrick.sirref.org/) for suggesting the name, and all the developers of the dependencies used, especially [icalendar](https://github.com/robur-coop/icalendar) and [timere](https://github.com/daypack-dev/timere).

