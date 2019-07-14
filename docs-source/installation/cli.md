# CLI Tool

## Configuration

It's a good idea to create a config file
at `~/.config/tasklite/config.yaml` first.

Check out the [example config file] for infos about available settings.

[example config file]:
  https://github.com/ad-si/TaskLite/blob/master/tasklite-core/example-config.yaml


## From Source

To build TaskLite from source, you need [Stack].

[Stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/

```shell
git clone https://github.com/ad-si/TaskLite
cd TaskLite
stack install tasklite-core
```

To test the installation run:

```shell
tasklite version
```


## With Docker

If you just want to try it out run:

```sh
docker run --rm adius/tasklite sh
tasklite help
```

When exiting the container all data will be discarded.

For repeated local usage run following command,
but make sure to replace `$TASKLITE_PATH` with the path to your
TaskLite installation as defined in your `config.yaml` file.
Per default it's created in the [XDG base directory]:
`$HOME/.local/share/tasklite`.

[XDG base directory]: https://standards.freedesktop.org/basedir-spec/latest/

```sh
docker run \
  --rm \
  --volume "$TASKLITE_PATH":/root/.local/share/tasklite \
  adius/tasklite
```

To make it easier to use, create an alias like:

```sh
alias tl="docker run …"
```

Providing your own `config.yaml` file to the docker container
is not yet supported.
