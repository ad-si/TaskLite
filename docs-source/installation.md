# Installation

## CLI Tool

### From Source

To build TaskLite from source, you need [Stack].

[Stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/

```shell
git clone https://github.com/ad-si/TaskLite
cd tasklite
stack install tasklite-core
```

To test the installation run:

```shell
tasklite version
```


### With Docker

**The Docker image is unfortunately currently broken, but I'm working on it!**

```sh
docker run \
  --rm \
  --volume ~/tasklite:/root/tasklite \
  adius/tasklite
```

To make it easier to use create following alias:

```sh
alias tl="docker run --rm --volume ~/tasklite:/root/tasklite adius/tasklite"
```


## Desktop App

### Native GTK App

**Attention: This is still early alpha**

A few dependencies must be availabe to build the app.
To install them on macOS run:

```shell
brew install \
  gtk+3 \
  libffi \
  gobject-introspection \
  gdk-pixbuf
```

```shell
git clone https://github.com/ad-si/TaskLite
cd tasklite
stack install tasklite-app
```

It might be necessary to add the package "libffi" to the pkg-config search path
before installation.
For example with [fish]:
```fish
set -x PKG_CONFIG_PATH /usr/local/opt/libffi/lib/pkgconfig
```

Start it with:
```sh
tasklite-app
```

[fish]: https://fishshell.com/


### DB Browser for SQLite

Alternatively you can use the [DB Browser for SQLite]
to view and modify your tasks directly in the SQLite database.

[DB Browser for SQLite]: https://sqlitebrowser.org/


### Web Interface

The web interface is currently based on [Datasette] and can
only be used to view tasks, but not to create new ones.

In combination with the Docker container
the web frontend for the SQLite database can be served in following way:

[Datasette]: https://github.com/simonw/datasette

```sh
docker run \
  --rm \
  --entrypoint datasette \
  --publish 8001:8001 \
  --volume ~/TaskLite:/root/tasklite \
  --volume "$PWD"/datasette:/root/datasette \
  adius/tasklite \
  serve \
    --host 0.0.0.0 \
    --metadata /root/datasette/metadata.json \
    --reload \
    /root/tasklite/main.db
```

**Attention:** Make sure that the IP address matches with your host's.

There is a predefined query for a `tl head` like overview:
<http://0.0.0.0:8001/main/tasks_pretty>

Generate custom view by appending the SQL query to
<http://0.0.0.0:8001/main?sql=>.
For example <http://0.0.0.0:8001/main?sql=select%20\*%20from%20tasks>.


Some example views:

Equivalent to `tl head`:
```sql
select substr(ulid,22) as ulid,priority,body,due_utc,
  replace(tags,',',', ') as tags,notes,user
from tasks_view
where closed_utc is null
order by priority desc
limit 50
```

Make sure to bookmark the views for easy access.
