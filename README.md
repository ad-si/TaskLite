# TaskLite

CLI task-list manager built with Haskell and SQLite.

<img
  src='screenshots/withtag.svg'
  alt="Screenshot of all TaskLite related tasks in the maintainer's database"
  width='600'
/>


## Installation

```shell
stack install tasklite
```

## Features / TODO

- Multi User
  - Add a `created_by` field to each entry
- Burndown Chart


## Differences to Taskwarrior

- **Simpler** \
  Taskwarrior has several redundant features and unnecessarily reimplements
  shell features like aliases.

- **More Robust & Stable** \
  Taskwarrior is plagued by [numerous bugs][TW Issues] due to its
  unncessary complexity and nonoptimal choice of programming languages.
  TaskLite's simple structure and Haskell's excellent correctness guarantees,
  however, yield to a very stable and robust piece of software.

- **Faster** \
  Haskell plus SQLite yields to an outstanding performance.

- **More Powerful** \
  As all tasks are stored in a SQLite database, you can use all the available
  tooling for it to supercharge your TaskLite installation.
  For example [Datasette] for an instant REST API, or [DB Browser for SQLite]
  to manipulate and view your data in a GUI.

[TW Issues]: https://github.com/GothenburgBitFactory/taskwarrior/issues
[Datasette]: https://github.com/simonw/datasette
[DB Browser for SQLite]: https://sqlitebrowser.org


## Manual

### Help

```shell
tasklite help
```

<img
  src='screenshots/recording.svg'
  alt='Screenshot of TaskLite help page'
  width='600'
/>


### Add

To add a task run

```shell
tasklite add Improve the TaskLite manual
```

It is also possible to immediately add tags when creating a task:

```shell
tasklite add Improve the TaskLite manual +tasklite +pc
```

(Attention: The tags must be the last parameters)


### Context / Views

For example I have following `work` command in my `$PATH`:

```bash
#! /usr/bin/env bash

tasklite query \
  "(tags is null or tags not like '%feram%') \
    and state is 'Open' \
    order by priority \
    desc limit 10"
```


### Import / Export

Import a GitHub issue:

```
curl https://api.github.com/repos/$OWNER/$REPO/issues/$NUM | tl import
```


### Exports Fields Matrix

*Implicit State* | Open | Waiting | Repeating | Done   | Closed
state            | -    |         |           | Done   | Obsolete
-----------------|------|---------|-----------|--------|---------
ulid             | ✅   |   ✅    |   ✅     |   ✅   |   ✅
wait_utc         |      |   ✅    |   ✅     |   ❔   |   ❔
closed_utc       |      |         |          |   ✅   |   ✅
priority         | 🛠   |   🛠    |   🛠     |   🛠   |   🛠

Legend:
- [ ] = Not allowed
- ✅ = Required
- ❔ = Maybe
- 🛠 = Generated


### REST API

Powered by [Datasette]:

```shell
datasette serve ~/tasklite/main.db
```

```shell
curl --location http://127.0.0.1:8001/main/tasks_view.json
```


## Development

### Generate Screenshot

Use asciinema to generate the terminal recording:

```sh
asciinema rec \
  --title 'TaskLite Help Page' \
  --command 'tasklite help' \
  --overwrite \
  screenshots/recording.json
```

```sh
asciinema rec \
  --title 'TaskLite "withtag" Command' \
  --command 'tasklite withtag tasklite' \
  --overwrite \
  screenshots/withtag.json
```

Change the size of the terminal in the recording.json file to:

```json
  "width": 80,
  "height": 86,
```

Then use [svg-term] to generate the SVG image:

```sh
svg-term \
  --no-cursor \
  --at 99999 \
  --window \
  --term iterm2 \
  --profile ../../dotfiles/terminal/adius.itermcolors \
  < screenshots/recording.json \
  > screenshots/recording.svg
```

[svg-term](https://github.com/marionebl/svg-term-cli)
