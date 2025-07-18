# Desktop App

---
<!-- toc -->
---


## Native GTK App

**Attention: This is still early alpha**

A few dependencies must be available to build the app.
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
cd TaskLite
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


## DB Browser for SQLite

Alternatively, you can use the [DB Browser for SQLite]
to view and modify your tasks directly in the SQLite database.

[DB Browser for SQLite]: https://sqlitebrowser.org/
