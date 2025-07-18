# CLI Tool

---
<!-- toc -->
---

## Configuration

It's a good idea to customize your config file
at `~/.config/tasklite/config.yaml` before starting to use TaskLite.

Check out the [example config file] for infos about available settings.

[example config file]:
  https://github.com/ad-si/TaskLite/blob/master/tasklite-core/example-config.yaml


## Help

For a full overview of all supported subcommands run:

```sh
tasklite help
```

![Screenshot of CLI output of `help` command](../images/help.svg)


## Add

To add a task run:

```shell
tl add Improve the TaskLite manual
```

It is also possible to immediately add tags when creating a task:

```shell
tl add Improve the TaskLite manual +tasklite +pc
```

And even to set certain fields:

```shell
tl add Buy milk +groceries due:2020-09-01 created:2020-08-27
```

**Attention:**
The tags and special commands must be the last parameters,
but their order doesn't matter.


## Edit

Existing tasks can be easily edited in their YAML representation:

```sh
tl edit 01hwcw6s1kzakd5pje218zmtpt
```

This will open your default editor,
as specified in the environment variables `$VISUAL` or `$EDITOR`.
You can then easily edit existing fields and add new fields.

This feature allows for a powerful batch editing workflow.
Use an SQL query to select a subsection of your tasks
and then edit all of them one by one:

```bash
sqlite3 \
  ~/TaskLite/main.db \
  "SELECT ulid FROM tasks WHERE metadata LIKE '%sprint%'" \
| while read ulid; do tl edit $ulid; done
```


## List and Filter

There are several commands to list and filter tasks.

For example:

- `tl all` - Lists all tasks.
- `tl new` - List newest tasks by creation UTC descending.
- `tl notag` - List tasks without tags.
    This is commonly used as a kind of inbox.
    If you want to capture a thought or idea,
    use this command to quickly add an entry and then later tag it properly.
- `tl random` - Prints a random task.

The most powerful command is `tl get`, which allows you to filter tasks
using an expressive filter language.

- `tl get +chore` - Lists all tasks with the `chore` tag.
- `tl get state:done` - Lists all completed tasks.

Those can also be combined:

```sh
tl get +chore state:done
```

There are several other commands that support filter expressions.
E.g. `tl random +chore` prints a random task with the `chore` tag.


### Advanced Filtering

For advanced use-cases TaskLite supports exporting tasks in various formats
so you can use other tools to further filter, process, and analyze them.

Among them are the `csv`, `json`, `ndjson`, `runsql`, and `query` commands.

E.g. following example prints all tasks sorted by their number of tags:

```sh
tl ndjson \
| jq --raw-output '(.tags | length | tostring) + " " + .ulid' \
| sort --numeric-sort --reverse
```

Make sure to check out their help pages for more information.


## Context / Views

There is no first class support for views (or "context" in [GTD] slang),
because it can be easily implemented with aliases / custom CLI commands
and the SQL query API.

[GTD]: https://en.wikipedia.org/wiki/Getting_Things_Done

For example, I have following `work` command in my `$PATH`:

```bash
#! /usr/bin/env bash

tasklite query \
  "(tags is null or tags like '%work%') \
    and closed_utc is null \
    order by priority desc, due_utc asc, ulid desc \
    limit 10"
```


## Import

TaskLite features a comprehensive and robust JSON importer.

For example to import a GitHub issue simply run:

```sh
curl https://api.github.com/repos/$OWNER/$REPO/issues/$NUM | tl import
```

Or to import a task from TaskWarrior:

```sh
task 123 export | tl import
```

In order to avoid data loss of fields which aren't directly
supported by TaskLite, the whole imported JSON object is also stored
in TaskLite's task `metadata` field.
However, if the original JSON object already has a `metadata` field,
its value is used instead.

> [!WARNING]
> An import object's `tags` field must be of type `[string]`,
> while a `notes` field must be of type `{ulid?: string, body: string}`.

> [!WARNING]
> TaskLite does not properly support importing tasks
> which were created before 1970.
> While they can be imported, the creation date is set to 1970-01-01.


## Export

Use one of following commands:

- `tl csv`
- `tl ndjson`
- `tl backup` - Creates a backup at `$TaskLiteDir/backups/YYYY-MM-DDtHHMM.db`


## Custom Views

The export commands in combination with other common CLI tools like
[csvkit] can also be used for custom views of tasks.

[csvkit]: https://csvkit.readthedocs.io

```sh
tl csv \
| csvgrep --column tags --match tasklite \
| head -n 6 \
| csvcut --columns ulid,body,tags \
| csvlook --max-column-width 30
```

yields

```txt
| ulid                       | body                           | tags     |
| -------------------------- | ------------------------------ | -------- |
| 01chk64zwwjyybanvk7016hyyg | Add a burndown chart view      | tasklite |
| 01chk6c08h70xra2awd8dngtr7 | Add multi user support         | tasklite |
| 01chk6dxaxttwfyg019d3g3sze | Add a statistics view          | tasklite |
| 01chk6f3sq1mrskgkt1046fz7q | Add a calendar view            | tasklite |
| 01chk6vnm30ttvwc1qkasjaktm | Publish the TaskLite git re... | tasklite |
```


## Metadata

The metadata field allows you to store additional data for each task
which is not yet covered by TaskLite's core fields.
It is stored as a JSON object and therefore supports all JSON datatypes.

This is similar to Taskwarrior's
[User Defined Attributes](https://taskwarrior.org/docs/udas.html).

Metadata is especially useful for importing and migrating external tasks
without losing any information.

If you, for example, want to import following `task.json` file,
you will notice that it contains a field `kanban-state`, which has
no equivalent in TaskLite:

```json
{
  "created_at": "2020-02-08T20:02:32Z",
  "body": "Buy milk",
  "kanban-state": "backlog"
}
```

However, you can still simply import it, as the additional field
will be stored in the metadata object:

```txt
$ tl import < task.json
📥 Imported task "Buy milk" with ulid "01e0k6a1p00002zgzc0845vayw"
```

Inspecting it:

```txt
$ tl info 01e0k6a1p00002zgzc0845vayw
awake_utc: null
review_utc: null
state: null
repetition_duration: null
recurrence_duration: null
body: Buy milk
user: adrian
ulid: 01e0k6a1p00002zgzc0845vayw
modified_utc: 1970-01-01T00:00:00Z
group_ulid: null
closed_utc: null
priority_adjustment: null
metadata:
  body: Buy milk
  kanban-state: backlog
  created_at: 2020-02-08T20:02:32Z
waiting_utc: null
ready_utc: null
due_utc: null
priority: 0.0
tags:

notes:
```

To access the the metadata key programmatically you can do following:

```txt
$ tl ndjson \
  | grep 01e0k6a1p00002zgzc0845vayw \
  | jq -r '.metadata["kanban-state"]'
backlog
```

Or leverage SQL:

```txt
$ tl runsql "
    SELECT json_extract(metadata, '\$.kanban-state')
    FROM tasks
    WHERE ulid == '01e0k6a1p00002zgzc0845vayw'
  " \
  | tail -n 1
backlog
```

This can also be used to update metadata fields:

```txt
tl runsql "
    UPDATE tasks
    SET metadata=(
      SELECT json_set(tasks.metadata, '\$.kanban-state', 'sprint')
      FROM tasks
      WHERE ulid == '01e0k6a1p00002zgzc0845vayw'
    )
    WHERE ulid == '01e0k6a1p00002zgzc0845vayw'
  "
```

Another great use-case is to automatically extract and store data
from the body of the task.

E.g. you could set up a CRON job to automatically extract any GitHub link
from the body and store it in an extra `github_url` metadata field:

```sql
UPDATE tasks
SET metadata = json_insert(
  ifnull(metadata, '{}'),
  '$.github_url',
  substr(body, instr(body, 'https://github.com'),
    CASE
      WHEN instr(substr(body, instr(body, 'https://github.com')), ' ') == 0
      THEN length(substr(body, instr(body, 'https://github.com')))
      ELSE instr(substr(body, instr(body, 'https://github.com')), ' ') - 1
    END
  )
)
WHERE body LIKE '%https://github.com%'
```


<!-- TODO:
Soon TaskLite will also support a dedicated `metadata` command like:

```sh
tl metadata get kanban-state 01e0k6a1p00002zgzc0845vayw
```

and

```sh
tl metadata set kanban-state sprint 01e0k6a1p00002zgzc0845vayw
```
-->


## External Commands

Like Git, TaskLite also supports external commands.
This allows you to easily extend TaskLite's functionality with your own scripts.

For this to work, simply add an executable script (`chmod +x`)
with the prefix `tasklite-` to your `$PATH`

For example, to add a `grin` command which simply prints a smiley:

```sh
$ cat /usr/local/bin/tasklite-grin
#! /usr/bin/env bash

echo '😁' "$@"

$ tasklite grin Hi
😁 Hi
```
