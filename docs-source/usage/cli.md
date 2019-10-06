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
