# Hooks

Hooks are scripts that can be executed
at various stages of TaskLite's execution.

---
<!-- toc -->
---

## Configuration

Hooks can either be specified via the config file or via hook files.

If the files have an extension (e.g. `post-add.lua`) the corresponding
interpreter will be used.
Otherwise the file will be executed as a shell script.
Currently supported interpreters are: `lua`, `python3`, `ruby`, `node`.

If the hook files are shell scripts, they must be executable (`chmod +x`).
Otherwise they can't be executed directly by TaskLite.

It's recommended to use [Lua](https://www.lua.org/) for hooks
as it's simple, lightweight, and has the best performance.
Futhermore, future versions of TaskLite will include a Lua interpreter
for even better performance.


## Stages

Following stages are available:

- Launch
  - `pre-launch` - After reading all configs,
      but before any TaskLite code is executed.
      Can be used to prevent execution of TaskLite.
  - `post-launch` - After reading CLI arguments,
      setting up the database and running all migrations.
- Add
  - `pre-add` - Right before adding a new task.
      Can be used to prevent addition of task.
  - `post-add` - After new task was added.
- Modify
  - `pre-modify` - Right before a task gets modified.
      Can be used to prevent modification of task.
  - `post-modify` - After task was modified.
- Exit
  - `pre-exit` - Pre printing results
  - `post-exit` - Last thing before program termination

The hooks receive JSON data from TaskLite via stdin.
We're using JSON5 here for better readability.

Included fields are:

```js
{
  arguments: […],  // Command line arguments (after `tasklite`)
  …  // Stage specific fields (see below)
}
```

After execution, a called hook can print a JSON object to stdout.
All fields of the JSON are optional.

Possible values:

```js
{
  message: "…",  // A message to display on stdout
  warning: "…",  // A warning to display on stderr
  error: "…",  // An error to display on stderr
  …  // Any other fields you want to include
}
```

Hooks can write to stdout at any time, but it's not recommended.
Rather write a `{ message: "…" }` object to stdout and
let TaskLite print the message with improved formatting and coloring.
Same goes for stderr.

<small>
<table>
  <thead>
    <tr>
      <th>Event</th>
      <th>Stdin</th>
      <th>Stdout on Success<br>(exitcode == 0)</th>
      <th>Stdout on Error<br>(exitcode != 0)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>pre&#8209;launch</code></td>
      <td>❌</td>
      <td><pre>{ message: "…", … }</pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;launch</code></td>
      <td><pre>{ arguments: […] }</pre></td>
      <td><pre>{ message: "…", … }</pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;add</code></td>
      <td><pre>
{
  arguments: […],
  taskToAdd: {}
}
      </pre></td>
      <td><pre>
{
  taskToAdd: {},
  message: "…",
  …,
}
      </pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;add</code></td>
      <td><pre>
{
  arguments: […],
  taskAdded: {}
}
      </pre></td>
      <td><pre>{ message: "…", … }</pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;modify</code></td>
      <td><pre>
{
  arguments: […],
  taskOriginal: {}
}
      </pre></td>
      <td><pre>
{
  taskModified: {},
  message: "…",
  …
}
      </pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;modify</code></td>
      <td><pre>
{
  arguments: […],
  taskOriginal: {},
  taskModified: {}
}
      </pre></td>
      <td><pre>
{
  taskModified: {},
  message: "…",
  …
}
      </pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;exit</code></td>
      <td><pre>❌</pre></td>
      <td><pre>{ message: "…", … }</pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;exit</code></td>
      <td><pre>❌</pre></td>
      <td><pre>{ message: "…", … }</pre></td>
      <td>
        <pre>{ message: "…", … }</pre>
        <small>Processing terminates</small>
      </td>
    </tr>
  </tbody>
</table>
</small>


## Debugging

To see the JSON for a single task run:

```sh
tl ndjson | grep $ULID_OF_TASK | head -n 1 | jq
```


## Examples

### Shell

**Pre launch:**

```sh
stdin=$(cat)

>&2 echo "File > pre-launch: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```

**Post launch:**

```sh
stdin=$(cat)

>&2 echo "File > post-launch: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```

**Pre add:**

```sh
stdin=$(cat)

>&2 echo "File > pre-add: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```
