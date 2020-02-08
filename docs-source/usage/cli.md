# CLI Tool

## Add

To add a task run:

```shell
tl add Improve the TaskLite manual
```

It is also possible to immediately add tags when creating a task:

```shell
tl add Improve the TaskLite manual +tasklite +pc
```

**Attention:** The tags must be the last parameters


## Help

For a full overview of all supported subcommands run:

```sh
tasklite help
```

![Screenshot of CLI output of `help` command](../images/help.svg)


## Context / Views

There is no first class support for views (or "context" in [GTD] slang),
because it can be easily implemented with aliases / custom CLI commands
and the SQL query API.

For example I have following `work` command in my `$PATH`:

```bash
#! /usr/bin/env bash

tasklite query \
  "(tags is null or tags not like '%feram%') \
    and state is 'Open' \
    order by priority \
    desc limit 10"
```


## Analyze and Filter Tasks

In order to further analyze and filter tasks TaskLite includes the
`ndjson` command, which prints all tasks as newline delimited JSON objects.

This output can then easily be analyzed and filtered with standard UNIX tools.
E.g. following example prints all tasks related to music:

```sh
tl ndjson | grep 'music' | jq
```


## Import

Import a GitHub issue:

```
curl https://api.github.com/repos/$OWNER/$REPO/issues/$NUM | tl import
```


## Export

Use one of following commands:

- `tl csv`
- `tl ndjson`
- `tl backup` - Creates a backup at `$TaskLiteDir/backups/YYYY-MM-DDtHHMM.db`


[GTD]: https://en.wikipedia.org/wiki/Getting_Things_Done


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
    select json_extract(metadata, '\$.kanban-state')
    from tasks
    where ulid == '01e0k6a1p00002zgzc0845vayw'
  " \
  | tail -n 1
backlog
```

This can also be used to update metadata fields:

```txt
tl runsql "
    update tasks
    set metadata=(
      select json_set(tasks.metadata, '$.kanban-state', 'sprint')
      from tasks
      where ulid == '01e0k6a1p00002zgzc0845vayw'
    )
    where ulid == '01e0k6a1p00002zgzc0845vayw'
  "
```

&nbsp;

Soon TaskLite will also support a dedicated `metadata` command like:

```sh
tl metadata get kanban-state 01e0k6a1p00002zgzc0845vayw
```

and

```sh
tl metadata set kanban-state sprint 01e0k6a1p00002zgzc0845vayw
```
