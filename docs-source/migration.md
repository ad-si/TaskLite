# Migration

This is a best effort list on how to migrate your tasks
from other task managers to TaskLite.


## YAML File

If you have all you tasks in one YAML file like this:

```yaml
- id: 123
  body: Buy milk
  tags: [groceries]

- id: 456
  body: Go running
  tags: [sport]
```

Run following command to import it.
Be sure to make [yaml2json] available in your path
and to install [jq] first.

[yaml2json]: https://github.com/ad-si/dotfiles/blob/master/bin/yaml2json
[jq]: https://stedolan.github.io/jq/

```bash
cat tasks.yaml \
| yaml2json \
| jq -c '.[]' \
| while read -r task
  do
    echo "$task" | tl import
  done
```


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


## Google Keep

You can export all tasks / notes from Google Keep via [Google Takeout].
It provides a `Takeout/Keep` directory
with one `.html` and `.json` file per task.

To import the `.json` files,
change into the directory and run following command:

```bash
find . -iname '*.json' \
| while read -r task
  do
    jq -c \
      '.textContent as $txt
        | .labels as $lbls
        | {
            utc: .userEditedTimestampUsec,
            body: .title
          }
        | if $lbls      then . + {tags: ($lbls | map(.name))} else . end
        | if $txt != "" then . + {notes: [{body: $txt}]}        else . end
      ' \
      "$task" \
    | tl import
  done
```

The title of the Google Keep note becomes the body of the task
and the note itself becomes a TaskLite note attached to the task.

[Google Takeout]: https://takeout.google.com
