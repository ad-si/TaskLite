# Migration

This is a best effort list on how to migrate your tasks
from other task managers to TaskLite.


## Taskwarrior

TaskLite supports all fields of Taskwarrior's [export format].
Therefore migration is really simple:

```bash
task export rc.json.array=off \
| while read -r task; do echo $task | tasklite import; done
```

[export format]: https://taskwarrior.org/docs/design/task.html


## Google Tasks

There is currently no proper way to export tasks.

A workaround is:

1. Open the [standalone view of Google Tasks][gt]
1. Select all text with `cmd + a` and copy it
1. Paste it in a text editor
1. Format it properly
1. Import it with a `while` loop as seen in the Taskwarrior section

[gt]: https://tasks.google.com/embed/?origin=https://calendar.google.com&fullWidth=1
