![Logo banner](./images/banner@2.png)

# Introduction

TaskLite is a CLI task manager built with [Haskell] and [SQLite].

[Haskell]: https://haskell.org
[SQLite]: https://sqlite.org

```txt
$ tl add Buy milk +groceries
🆕 Added task "Buy milk" with id "01dd62xryn5fnzjgynkcy06spb"

$ tl add Go running
🆕 Added task "Go running" with id "01dd62yjtrtmaph23knff6mbsj"

$ tl
Id  Prio  Opened UTC  Body
pb   2    2019-06-12  Buy milk  +groceries
sj   0    2019-06-12  Go running

$ tl do pb
✅ Finished task "Buy milk" with id "01dd62xryn5fnzjgynkcy06spb"
```

[Code on GitHub]

[Code on GitHub]: https://github.com/ad-si/tasklite


## Why Another CLI Task Manager?

[Taskwarrior] has been the gold standard for CLI task managers so far.
However, I repeatedly lost tasks due to [weird bugs]
and syncing issues.
I also found several UI decisions inept and wanted something
with a better workflow.
But probably most importantly I couldn't see myself contributing
to a C++ project.
I had been working with C++ at university and it wasn't pleasant.

To sum it up: I finally wanted something which I could fully own
and use until the end of days.
That means:

- Does not suddenly get bought by a bigger fish and get closed down
    or made unusable (looking at you [Wunderlist])
- Is written in a high-performance programming language,
    yet gives me lot's of guarantees about the code's stability
    and makes it easy for other developers to contribute
- Free software
- With a stable, future proof, powerful, and fast backend
    (currently [SQLite], but support for plain files and Git is planned)

[Wunderlist]:
  https://www.theverge.com/2018/3/21/17146308/microsoft-wunderlist-to-do-app-acquisition-complicated
[Taskwarrior]: https://taskwarrior.org
[weird bugs]: https://github.com/GothenburgBitFactory/taskwarrior/issues/1831


## Why Not [Org-mode]?

I don't like the unstructured combination of outlining, notes and tasks.
Furthermore I don't like interactive document editing UIs in the terminal.
I prefer REPL style apps which adhere to UNIX conventions and let
me compose them easily with other CLI tools.

This, however, is just a personal preference
and otherwise [Org-mode] is certainly a good solution.
Also check out [Smos],
which is another powerful tree-based editor with extra focus
on [Getting Things Done].

[Org-mode]: https://orgmode.org/
[Smos]: https://smos.cs-syd.eu/
[Getting Things Done]: https://en.wikipedia.org/wiki/Getting_Things_Done
