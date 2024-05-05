# Web Based GUIs

---
<!-- toc -->
---

## TaskLite Web App

The TaskLite web app is a simple [Elm Land](https://elm.land/) app
backed by an [AirGQL](https://github.com/Airsequel/AirGQL) GraphQL server.

![Screenshot of web app](../images/webapp_screenshot.png)

To use it you first need to start the server:

```sh
tasklite server
```

Then you need to start the web app server:

```sh
git clone https://github.com/ad-si/TaskLite
cd TaskLite/tasklite-webapp
make start
```

The web app will then be available at
[localhost:3000](http://localhost:3000).


## SQLite Web Frontends

### Datasette

[Datasette] is an open source multi-tool for exploring and publishing data.
It provides a web frontend for SQLite databases that
can be used to explore your tasks in following way:

[Datasette]: https://github.com/simonw/datasette

```sh
datasette ~/TaskLite/main.db
```

You can then use any of TaskLite's predefined views:
[127.0.0.1:8001/main/tasks_view](
  http://127.0.0.1:8001/main/tasks_view)

Generate custom view by appending the SQL query to the URL.
For example:
[0.0.0.0:8001/main?sql=SELECT+*+FROM+tasks+WHERE+body+LIKE+%27%25xyz%25%27](
  http://0.0.0.0:8001/main?sql=SELECT+*+FROM+tasks+WHERE+body+LIKE+%27%25xyz%25%27
)

You can then bookmark those views for easy access.

![Screenshot of web app](../images/datasette-ui_shadow.png)


### SQLite Web

Another way to host a simple web frontend is
[SQLite Web](https://github.com/coleifer/sqlite-web).

You can run it with Docker like this:

```sh
docker run -it --rm \
  -p 8080:8080 \
  -v ~/TaskLite:/data \
  -e SQLITE_DATABASE=main.db \
  coleifer/sqlite-web
```
