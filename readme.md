# TaskLite

CLI task-list manager built with [Haskell] and [SQLite].

<img
  src='tasklite-core/screenshots/withtag.svg'
  alt="Screenshot of all TaskLite related tasks in the maintainer's database"
  width='600'
/>


## Installation

In order to install the CLI tool, run:

```shell
stack install tasklite
```

or

```shell
docker pull adius/tasklite
```

For installation instructions for the app, API, and website,
please check out the corresponding directory.


## Usage

To run TaskLite in a container:

```sh
docker run --rm --volume ~/tasklite:/root/tasklite adius/tasklite
```
