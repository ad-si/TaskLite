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

Currently supported interpreters are:
`lua`, `python3`, `ruby`, `node`, and [`v`][vlang].
It's recommended to write you scripts in the [V programming language][vlang]
due to its high performance (script is compiled on first execution),
its great ergonomics, and its comprehensive standard library.
The compiled versions of the script will be cached as
`_v_executable_.<name-of-script>` in the hooks directory.
?
[vlang]: https://vlang.io

Another good alternative is [Lua](https://www.lua.org/)
as it is simple, lightweight, and fast.
Futhermore, future versions of TaskLite will include the Lua interpreter
to make it independent of the system's installed Lua interpreter.

If the hook files are shell scripts, they must be executable (`chmod +x`).
Otherwise they can't be executed directly by TaskLite.

The file names must start with the stage they are for (`pre` or `post`),
followed by a dash and the stage name (e.g. `pre-add`), and optionally
followed by an underscore and a description.
E.g `pre-add_validate_task.v` or `pre-exit_sync.v`.

They are executed in alphabetical order.
If you want to ensure a specific order,
you can include a number in the description.
E.g. `pre-add_01_x.v`, `pre-add_02_y.v`, ….

> [!WARNING]
> Multiple `pre-add` hooks are not supported yet,
> but will be in the future.

To ignore a hook, you can prefix the file name with an underscore (`_`).


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
  - `pre-exit` - Last thing before program termination

The hooks receive JSON data from TaskLite via stdin.
We're using JSON5 here for better readability.

Included fields are:

```js
{
  arguments: […],  // Command line arguments (after `tasklite`)
  …  // Stage specific fields (see below)
}
```

During execution, a called hook should print a result JSON object to stdout.
All fields of the JSON are optional.

Possible values:

```js
{
  message: "…",  // A message to display on stdout
  warning: "…",  // A warning to display on stderr
  error: "…",  // An error to display on stderr
  …  // Other fields depending on hook type (check out table below)
}
```

Hooks can write to stderr at any time, but it's not recommended.
Rather write a JSON object to stdout and
let TaskLite print the message / warning / error
with improved formatting and coloring.

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
  task: {},
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
      <td><pre>
{ message: "…", … }
      </pre></td>
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
  taskToModify: {}
}
      </pre></td>
      <td><pre>
{
  task: {},
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
{ message: "…", … }
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
  </tbody>
</table>
</small>


## Debugging

To see the JSON for a single task run:

```sh
tl ndjson | grep $ULID_OF_TASK | head -n 1 | jq
```
