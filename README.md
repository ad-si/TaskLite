# TaskLite

CLI task-list manager built with Haskell and SQLite.

## Installation

```shell
stack install tasklite
```

## REST API

Powered by [Datasette](https://github.com/simonw/datasette):

```shell
datasette serve ~/tasklite/main.db
```

```shell
curl --location http://127.0.0.1:8001/main/tasks_view.json
```
