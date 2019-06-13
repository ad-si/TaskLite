# REST API

The REST API is currently provided by [Datasette].

[Datasette]: https://github.com/simonw/datasette

All web views can be configured to deliver JSON
by simply changing the file extension in the URL to `.json`.

For example:
```shell
curl --location http://0.0.0.0:8001/main/tasks_view.json
```
