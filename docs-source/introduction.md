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

The Code is available on [GitHub].

[GitHub]: https://github.com/ad-si/tasklite


For help and ideas please come visit us at our [GitHub Discussions]!

[GitHub Discussions]: https://github.com/ad-si/TaskLite/discussions


### Latest Versions

- CLI {{#include ../tasklite-core/package.yaml:2}}
- App {{#include ../tasklite-app/package.yaml:2}}

[Full Changelog](/changelog.html)
