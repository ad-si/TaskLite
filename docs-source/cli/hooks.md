# Hooks

Hooks can either be specified via the config file
or via hook files.
But make sure that all hook files are executable,
otherwise they won't be picked up by TaskLite.

---
<!-- toc -->
---


## Stages

Following stages are available:

- `pre-launch` - After reading all configs,
    but before any TaskLite code is executed.
    Can be used to prevent execution of TaskLite.
- `post-launch` - After reading CLI arguments,
    setting up the database and running all migrations.
- `pre-add` - Right before adding a new task.
    Can be used to prevent addition of task.
- `post-add` - After new task was added.
- `pre-modify` - Right before a task gets modified.
    Can be used to prevent modification of task.
- `post-modify` - After task was modified.
- `pre-exit` - Pre printing results
- `post-exit` - Last thing before program termination

<!--
TODO:
- `pre-delete`
- `post-delete`
- `pre-review`
- `post-review`
-->

The hooks receive data from TaskLite via stdin.
Possible fields are:

```json5
{
  arguments: […],  // Command line arguments (after `tasklite`)
  taskOriginal: {},  // Task before any modifications by TaskLite
  taskModified: {},  // Modified task
}
```

After execution, every called hook must print a JSON object to stdout
(even if it's empty).
All fields of the JSON are optional.

Explanation of possible values:

```json5
{
  message: "…",  // A message to display on stdout
  taskModified: "…",  // New version of the task as computed by your script
  tasksToAdd: […],  // Additional tasks to add
}
```

Hooks can write to stderr at any time, but it is not recommended.
Rather write a `{message: ''}` object to stdout and
let TaskLite print the message with improved formatting and coloring.

Legend:

- ❌ = Not available
- `->` = Must return following object (fields optional) on stdout


<small>
<table>
  <thead>
    <tr>
      <th>Event</th>
      <th>Input</th>
      <th>Success<br>(exitcode == 0)</th>
      <th>Error<br>(exitcode != 0)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>pre&#8209;launch</code></td>
      <td>❌</td>
      <td><pre>
-> {
  message: "…",
}</pre></td>
      <td>
        <pre>-> {message: "…"}</pre>
        Processing terminates
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;launch</code></td>
      <td><pre>
{
  arguments: […]
}
      </pre></td>
      <td><pre>
{
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
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
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
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
{
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
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
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
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
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;exit</code></td>
      <td><pre>
{}
      </pre></td>
      <td><pre>
{
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;exit</code></td>
      <td><pre>
{}
      </pre></td>
      <td><pre>
{
  message: "…"
}
      </pre></td>
      <td>
        <pre>{stderr: "…"}</pre>
        Processing terminates
      </td>
    </tr>
  </tbody>
</table>
</small>

&nbsp;


To see the JSON for a single task run:

```sh
tl ndjson | head -n 1 | jq
```
