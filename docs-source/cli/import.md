# Import / Migration

This is a best effort list on how to import your tasks
from other task managers to TaskLite.

---
<!-- toc -->
---


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

You can import them directly:

```bash
cat tasks.yaml | tasklite importyaml
```

If you need to process tasks one by one, you can use [yaml2json] and [jq]:

[yaml2json]: https://github.com/ad-si/dotfiles/blob/master/bin/yaml2json-ruby
[jq]: https://stedolan.github.io/jq/

```bash
cat tasks.yaml \
| yaml2json \
| jq -c '.[]' \
| while read -r task
  do
    echo "$task" | tasklite importjson
  done
```


## Taskwarrior

TaskLite supports all fields of Taskwarrior's [export format].
Therefore, a migration is really simple:

```bash
task export | tasklite importjson
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
        | .title as $title
        | (if .isArchived then "done"
          elif .isTrashed then "deletable"
          else null
          end) as $state
        | {
            utc: .userEditedTimestampUsec,
            body: ((if $title and $title != "" then $title else $txt end)
              + (if .listContent
                then "\n\n" +
                  (
                    .listContent
                    | map("- [" + (if .isChecked then "x" else " " end) + "] "
                      + .text)
                    | join("\n")
                  )
                else ""
                end))

          }
        | if $lbls then . + {tags: ($lbls | map(.name))} else . end
        | if $title and $title != "" and $txt and $txt != ""
          then . + {notes: [{body: $txt}]}
          else .
          end
        | if $state then . + {state: $state} else . end
      ' \
      "$task" \
    | tl importjson
  done
```

The title of the Google Keep note becomes the body of the task
and the note itself becomes a TaskLite note attached to the task.
A list of sub-tasks will be converted to a GitHub Flavored Markdown [task list].

[task list]:
  https://help.github.com/en/github/writing-on-github/basic-writing-and-formatting-syntax#task-lists
[Google Takeout]: https://takeout.google.com


## Telegram

Telegram's "Saved Messages" -- aka messages to oneself -- are a pretty
convenient inbox. Here is how to move them to TaskLite afterward:

1. Install [Telegram Desktop](https://desktop.telegram.org/)
    `brew install telegram-desktop`
1. Go to "Saved Messages"
1. Click on 3 dots in the upper right corner
1. Click on "Export chat history"
1. Deselect all additional media and select "JSON" as output format
1. Approve download on a mobile device
1. Download the JSON file
    (if download window was closed, simply follow the previous steps again)
1. Either import it directly as JSON or convert it first to YAML for cleanup.

    Import JSON directly:
    ```bash
    cat result.json \
    | jq -c '
      .messages
        | map(
            (if (.text | type) == "string"
            then .text
            else (.text
                | map(
                    if (. | type) == "string"
                    then .
                    else .text end
                  )
                | join(", ")
              )
            end) as $body
            | {
              utc: .date,
              body: $body,
              tags: ["telegram"]
            }
          )
        | .[]
      ' \
    | while read -r task
      do
        echo "$task" | tasklite importjson
      done
    ```

    Convert it to YAML for easier cleanup:
    ```sh
    jq '.messages' result.json | yq --yaml-output > out.yaml
    ```
1. Clear chat history on Telegram


## Apple Reminders

Use following Apple Script to display the reminders
including their creation timestamp.
Seen at [discussions.apple.com/thread/8570915](
  https://discussions.apple.com/thread/8570915).

1. Download the script: [`export-reminders.scpt`](./export-reminders.scpt)
1. Run it with:
    ```sh
    osascript export-reminders.scpt
    ```
1. Enter the name of the list you want to export
    <!--
      TODO: Use alert after
            https://github.com/lambdalisue/rs-mdbook-alerts/issues/17
    -->
    > **⚠️ Warning** \
    > It includes *all reminders* - even completed ones - in the list.
    > If it's a long list, it will take a while.
    > A better approach would be to create a new list and move all
    > reminders you want to export to that list.
1. Copy and paste the output into a `tasks.json` file
1. Format it as proper JSON and manually add notes, and tags fields
1. Import JSON file:
    ```bash
    cat tasks.json | tasklite importjson
    ```

    Alternatively, if you need to process tasks one by one:
    ```bash
    cat tasks.json \
      | jq -c '.[]' \
      | while read -r task
        do
          echo $task | tasklite importjson
        done
    ```


## Fixing Mistakes

It's easy to write a short shell script to fix any mistakes
you might have made during the import process.

In following example I forgot to use the metadata field `end`
to set the `closed_utc` column, but used the current date instead.

As I deleted all metadata afterward,
I now need to extract the field from an old backup.
In the meantime I changed some `closed_utc` fields and therefore I can't fully automate it.
A neat little trick I came up with is to automatically paste the correct value
into the clipboard, so I only have to insert it at the right location.

A [fish](https://fishshell.com/) script to fix the mistake could look like this:

```fish
sqlite3 \
  ~/TaskLite/main.db \
  "SELECT ulid FROM tasks WHERE closed_utc LIKE '%2024-02-26%'" \
| while read ulid \
    ; echo "$ulid" \
    ; and \
      sqlite3 \
        ~/TaskLite/backups/2024-02-14t1949.db \
        "SELECT json_extract(metadata, '\$.end') FROM tasks WHERE ulid == '$ulid'" \
      | tee /dev/tty \
      | pbcopy \
    ; and tl edit "$ulid" \
  ; end
```
