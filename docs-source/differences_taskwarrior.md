## Differences to Taskwarrior

---
<!-- toc -->
---


### General

- **Simpler** \
  Taskwarrior has several redundant features and unnecessarily re-implements
  shell features like aliases.

- **More Robust & Stable** \
  Taskwarrior is plagued by [numerous bugs][TW Bugs] due to its
  unnecessary complexity and non-optimal choice of programming languages.
  TaskLite's simple structure and [Haskell]'s excellent correctness guarantees,
  however, yield a stable and robust piece of software.

- **Faster** \
  [Haskell] plus [SQLite] delivers excellent performance.
  Check out the  section about [performance] for a simple benchmark.

- **More Powerful** \
  As all tasks are stored in an [SQLite] database, so you can use the most
  daring SQL queries to extract hidden insights.
  E.g. What is the average completion time for a task created on Monday tagged
  "sprint7" created by user "john-evil"?

  Furthermore, extensive tooling is available for SQLite
  to supercharge your TaskLite installation.
  For example [Datasette] as an instant REST API, or [DB Browser for SQLite]
  to view, manipulate, and plot your tasks in a GUI.

  Other 3rd party tools to edit SQLite databases are:

  - [VisiData] - Interactive CLI multitool for tabular data.
  - [LiteCLI] - CLI SQLite browser with auto-completion and syntax highlighting.


[Datasette]: https://github.com/simonw/datasette
[DB Browser for SQLite]: https://sqlitebrowser.org
[Haskell]: https://haskell.org
[LiteCLI]: https://litecli.com
[performance]: /performance
[SQLite]: https://sqlite.org
[TW Bugs]: https://github.com/GothenburgBitFactory/taskwarrior/labels/bug
[VisiData]: https://visidata.org


### Command Comparison

TaskWarrior  | TaskLite   | Description & Differences
-------------|------------|--------------------------
[add]        | add        | Add a new task
annotate     | note       | Add a note / comment / annotation to a task
[append]     | - (edit)   | Append words to a task description
[calc]       | -          | Expression calculator
config       | config     | TL only displays it, TW allows modification
context      | -          | Manage contexts. TL uses tags instead.
[count]      | count      | Count the tasks matching a filter
delete       | trash      | Mark a task as deletable
denotate     | unnote     | Remove an annotation from a task
[done]       | do         | Complete a task
duplicate    | duplicate  | Clone an existing task
edit         | edit       | Launch your text editor to modify a task
execute      | -          | Execute an external command
[export]     | ndjson     | Export tasks in NDJSON instead of JSON format
help         | help       | Show high-level help, a cheat-sheet
import       | import     | Additionally to JSON supports Email files (.eml)
[log]        | log        | Record an already-completed task
logo         | -          | Show the Taskwarrior logo
[modify]     | - (edit)   | Modify one or more tasks
[prepend]    | - (edit)   | Prepend words to a task description
purge        | delete     | Completely remove task, rather than just change status
start        | start      | Start working on a task, make active
stop         | stop       | Stop working on a task, no longer active
[synchronize]| -          | Syncs tasks with Taskserver
undo         | -          | Revert last change
version      | version    | Version details and copyright
active       | -          | Started tasks
all          | all        | Pending, completed and deleted tasks
blocked      | -          | Tasks that are blocked by other tasks
blocking     | -          | Tasks that block other tasks
completed    | done       | Tasks that have been completed
[list]       | open       | Pending tasks
long         | -          | Pending tasks, long form
ls           | -          | Pending tasks, short form
minimal      | -          | Pending tasks, minimal form
newest       | new        | Most recent pending tasks
next         | head       | Most urgent tasks
oldest       | -          | Oldest pending tasks
overdue      | overdue    | Overdue tasks
ready        | ready      | Pending, unblocked, scheduled tasks
recurring    | recurring  | Pending recurring tasks
unblocked    | -          | Tasks that are not blocked
waiting      | waiting    | Hidden, waiting tasks
[burndown.daily]  | -     | Burndown chart, by day
[burndown.monthly]| -     | Burndown chart, by month
[burndown.weekly] | -     | Burndown chart, by week
calendar     | -          | Calendar and holidays
colors       | -          | Demonstrates all supported colors
[columns]    | -          | List of report columns and supported formats
commands     | help       | List of commands, with their behaviors
diagnostics  | -          | Show diagnostics, for troubleshooting
ghistory.annual | -       | History graph, by year
ghistory.monthly| -       | History graph, by month
ghistory.weekly | -       | History graph, by week
ghistory.daily  | -       | History graph, by day
history.annual  | -       | History report, by year
history.monthly | -       | History report, by month
history.weekly  | -       | History report, by week
history.daily   | -       | History report, by day
ids             | -       | Filtered list of task IDs
[information]   | info    | All attributes shown
projects        | projects| List of projects (project in TL = active tag)
reports         | help    | List of available reports
show            | config  | Filtered list of configuration settings
stats           | stats   | Filtered statistics
summary         | projects| Filtered project summary
tags            | tags    | Filtered list of tags
timesheet       | -       | Weekly timesheet report
udas            | -       | Details of all defined UDAs
uuids           | -       | Filtered list of UUIDs
\_aliases       | -       | List of active aliases
\_columns       | -       | List of supported columns
\_commands      | -       | List of supported commands
\_config        | -       | List of confguration setting names
\_context       | -       | List of defined context names
[\_get]         | -       | DOM accessor
\_ids           | -       | Filtered list of task IDs
\_projects      | -       | Filtered list of project names
\_show          | -       | List of `name=value` configuration settings
\_tags          | -       | Filtered list of tags in use
\_udas          | -       | List of configured UDA names
[\_unique]      | -       | List of unique values for the specified attribute
\_urgency       | -       | Filtered list of task urgencies
\_uuids         | -       | Filtered list of pending UUIDs
\_version       | -       | Task version (and optional git commit)
\_zshattributes | -       | Zsh formatted task attribute list
\_zshcommands   | -       | Zsh formatted command list
\_zshids        | -       | Zsh formatted ID list
\_zshuuids      | -       | Zsh formatted UUID list
-               | random  | Show a random open task


[add]: https://taskwarrior.org/docs/commands/add.html
[append]: https://taskwarrior.org/docs/commands/append.html
[calc]: https://taskwarrior.org/docs/commands/calc.html
[count]: https://taskwarrior.org/docs/commands/count.html
[done]: https://taskwarrior.org/docs/commands/done.html
[export]: https://taskwarrior.org/docs/commands/export.html
[log]: https://taskwarrior.org/docs/commands/log.html
[modify]: https://taskwarrior.org/docs/commands/modify.html
[prepend]: https://taskwarrior.org/docs/commands/prepend.html
[synchronize]: https://taskwarrior.org/docs/commands/synchronize.html
[Customizable reports]: https://taskwarrior.org/docs/report.html
[columns]: https://taskwarrior.org/docs/commandscolumns.html
[filter]: https://taskwarrior.org/docs/filter.html
[list]: https://taskwarrior.org/docs/commands/list.html
[burndown.daily]: https://taskwarrior.org/docs/commands/burndown.html
[burndown.monthly]: https://taskwarrior.org/docs/commands/burndown.html
[burndown.weekly]: https://taskwarrior.org/docs/commands/burndown.html
[columns]: https://taskwarrior.org/docs/commands/columns.html
[information]: https://taskwarrior.org/docs/commands/info.html
[\_get]: https://taskwarrior.org/docs/commands/_get.html
[\_unique]: https://taskwarrior.org/docs/commands/_unique.html
