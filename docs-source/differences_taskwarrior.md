## Differences to Taskwarrior

### General

- **Simpler** \
  Taskwarrior has several redundant features and unnecessarily re-implements
  shell features like aliases.

- **More Robust & Stable** \
  Taskwarrior is plagued by [numerous bugs][TW Issues] due to its
  unnecessary complexity and non-optimal choice of programming languages.
  TaskLite's simple structure and [Haskell]'s excellent correctness guarantees,
  however, yield a stable and robust piece of software.

- **Faster** \
  [Haskell] plus [SQLite] delivers an excellent performance.
  See section about [performance] for a simple benchmark.

- **More Powerful** \
  As all tasks are stored in an [SQLite] database, so you can use the most
  daring SQL queries to extract hidden insights.
  E.g. What is the average completion time for a task created on Monday tagged
  *sprint7* created by user *john-evil*?

  Furthermore, extensive tooling is available for SQLite
  to supercharge your TaskLite installation.
  For example [Datasette] for an instant REST APIs, or [DB Browser for SQLite]
  to view, manipulate, and plot your tasks in a GUI.

  Other 3rd Party Tools to Edit Tables are

  - [VisiData] - Interactive CLI multitool for tabular data.
  - [LiteCLI] - CLI SQLite browser with auto-completion and syntax highlighting.


[Datasette]: https://github.com/simonw/datasette
[DB Browser for SQLite]: https://sqlitebrowser.org
[Haskell]: https://haskell.org
[LiteCLI]: https://litecli.com
[performance]: /performance
[SQLite]: https://sqlite.org
[TW Issues]: https://github.com/GothenburgBitFactory/taskwarrior/issues
[VisiData]: http://visidata.org
