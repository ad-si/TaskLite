# TaskLite Thunderbird Add-On

Adds an "Add to TaskLite" entry to the context menus of Thunderbird's
message list and of the displayed message,
which creates a task from each selected or displayed email.
"Add to TaskLite and Edit" additionally opens each task in an editor window,
which uses the same format as `tasklite edit`.

- `extension/`: The add-on, packaged by `make thunderbird-addon`
- `src/`: Source of the editor window ([CodeMirror]),
    bundled to `extension/edit.bundle.js` with `npm run build`

[CodeMirror]: https://codemirror.net

The add-on sends the raw emails to `tasklite nativehost run`,
which imports them like `tasklite importeml`
and also provides the editable task and applies the edits.

Check out the [documentation](https://tasklite.org/automation.html#thunderbird-add-on)
for setup instructions.
