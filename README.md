# 📅 Caledonia 🏴󠁧󠁢󠁳󠁣󠁴󠁿

Caledonia is a command-line calendar client.
Currently, it operates on a [vdir](https://pimutils.org/specs/vdir/) directory of [`.ics`](https://datatracker.ietf.org/doc/html/rfc5545) files (as managed by tools like [vdirsyncer](https://github.com/pimutils/vdirsyncer)).

It has the `list`, `search`, `show`, `add`, `delete`, and `edit` subcommands, and has full timezone support.

An example `list` invocation is,

```
$ caled list
personal   2025-04-04 Fri 13:00 - 14:00 (America/New_York) New York 8am meeting      054bb346-b24f-49f4-80ab-fcb6040c19a7
family     2025-04-06 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
work       2025-04-09 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
personal   2025-04-10 Thu 11:00 - 12:00 (UTC)              Dentist                   ccef66cd4d1e87ae7319097f027f8322de67f758
family     2025-04-13 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-15 Tue - 2025-04-17 Thu                 John Doe in town          33cf18ec-90d3-40f8-8335-f338fbdb395b
personal   2025-04-15 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-16 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
personal   2025-04-19 Sat                                  Jane Doe's birthday       7hm4laoadevr1ene8o876f2576@google.com
family     2025-04-20 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-22 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-23 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
family     2025-04-27 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-29 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-30 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
```

See [TODO](./TODO.org) for future plans.

## Configuration

Caledonia looks for calendars in the directory specified by the `CALENDAR_DIR` environment variable or in `~/.calendars/` by default.

## Tests

The project includes a test suite that can be ran with `dune runtest`.

## Thanks

To [Patrick](https://patrick.sirref.org/) for suggesting the name, and all the developers of the dependencies used, especially [icalendar](https://github.com/robur-coop/icalendar) and [timere](https://github.com/daypack-dev/timere).

