# Hooks

- `pre-launch` - Before any TaskLite code is executed
- `post-launch`
- `pre-add`
- `post-add`
- `pre-modify`
- `post-modify`
- `pre-exit` - Pre printing results
- `post-exit` - Last thing before program termination

<!--
- `pre-delete`
- `post-delete`
- `pre-review`
- `post-review`
-->


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
{
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;launch</code></td>
      <td>❌</td>
      <td><pre>
{
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;add</code></td>
      <td><pre>
{
  command: "…",
  taskToAdd: {}
}
      </pre></td>
      <td><pre>
{
  taskToAdd: {},
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;add</code></td>
      <td><pre>
{
  command: "…",
  taskAdded: {}
}
      </pre></td>
      <td><pre>
{
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;modify</code></td>
      <td><pre>
{
  command: "…",
  taskOriginal: {}
}
      </pre></td>
      <td><pre>
{
  taskModified: {},
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;modify</code></td>
      <td><pre>
{
  command: "…",
  taskOriginal: {},
  taskModified: {}
}
      </pre></td>
      <td><pre>
{
  taskModified: {},
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>pre&#8209;exit</code></td>
      <td><pre>
{}
      </pre></td>
      <td><pre>
{
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
    <tr>
      <td><code>post&#8209;exit</code></td>
      <td><pre>
{}
      </pre></td>
      <td><pre>
{
  stdout: "…"
}
      </pre></td>
      <td>
        - Processing terminates
        <br>
        - <code>{stderr: "…"}</code>
      </td>
    </tr>
  </tbody>
</table>


&nbsp;


To see the JSON for a single task run:

```sh
tl ndjson | head -n 1 | jq
```
