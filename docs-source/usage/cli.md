# CLI Tool

## Add

To add a task run

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


## States

Instead of allowing one to explicitly set a state, TaskLite infers the
current state from several other fields.

There are 2 primary states:

- `Open` - Waits to be done
- `Closed` - Nothing left to be done

And 9 exclusive secondary states.

- `Asleep` - Is hidden because it's not relevant yet
- `Awake` - Has become relevant or will become soon
- `Ready` - Is ready to be done (similar to Open)
- `Waiting` - It's still unclear if the task needs to be done or really has been
    done. Regular checks are necessary until situation clears up.
- `Review` - It's necessary to check if the task can finally be started or
    if it has finally been completed.
- `Done` - Has been done
- `Obsolete` - Has become obsolete or impossible to finish
- `Deletable` - Not needed anymore and can be deleted (item in the trash)
- `Blocked` - Some other task(s) must be done first.
    Blockers are stored in a separate table.


State\Field|`awake_utc`|`ready_utc`|`waiting_utc`|`review_utc`|`closed_utc`|`state`
-----------|-----------|-----------|------------|-----------|-----------|--------
`Open`     | ❌        | ❌       |     ❌     | ❌        | ❌        | ❌
`Closed`   | ❌        | ❌       |     ❌     | ❌        | ✅        | ❌
`Asleep`   | > now     | > now     |     ❌     | ❌       | ❌         | ❌
`Awake`    | < now     | > now     |     ❌     | ❌       | ❌         | ❌
`Ready`    | < now     | < now     |     ❌     | ❌       | ❌         | ❌
`Waiting`  |   ❔      |   ❔     |     ✅     | > now     | ❌        | ❌
`Review`   |   ❔      |   ❔     |     ✅     | < now     | ❌        | ❌
`Done`     |   ❔     |   ❔      |     ❔     | ❔       | ✅         | `Done`
`Obsolete` |   ❔     |   ❔      |     ❔     | ❔       | ✅        |`Obsolete`
`Deletable`|   ❔     |   ❔      |     ❔     | ❔       | ✅        |`Deletable`
`Blocked`  |   ❔     |   ❔      |     ❔     | ❔       | ❌         | ❌

Legend:
- ❌ = Not allowed
- ✅ = Required
- ❔ = Maybe


Additional secondary states:

- `Repeating` - If this task get completed, a duplicate will be created
    with the specified time offset.
    I.e. subsequent tasks get delayed
    (e.g. mowing the lawn)
- `Recurring` - Task which needs to be done every day, week, etc.
    I.e. missed completions must be caught up immediately.
    (e.g. paying rent)
    The number of tasks which will be created in advance
    can be set via a config.


State\Field |`group_ulid`|`repetition_duration`|`recurrence_duration`
------------|------------|---------------------|---------------------
`Repeating`   | ✅       | ✅                 | ❌
`Recurring`   | ✅       | ❌                 | ✅


## Analyze and Filter Tasks

In order to further analyze and filter tasks TaskLite includes the
`ndjson` command, which prints all tasks as newline delimited JSON objects.

This output can then easily be analyzed and filtered with standard UNIX tools.
E.g. following example prints all tasks related to music:

```sh
tl ndjson | grep 'music' | jq
```


## Import / Export

Import a GitHub issue:

```
curl https://api.github.com/repos/$OWNER/$REPO/issues/$NUM | tl import
```


[GTD]: https://en.wikipedia.org/wiki/Getting_Things_Done
