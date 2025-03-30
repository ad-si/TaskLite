# Installation

---
<!-- toc -->
---

## Prebuilt Binaries

### MacOS

Install it via my [Homebrew](https://brew.sh) tap:

```sh
brew cask install ad-si/tap/tasklite
```

You can also get this (and previous) versions from
[the releases page](https://github.com/ad-si/TaskLite/releases).

Make sure to download the artifacts with `curl` or `wget`
as macOS prevents the execution of files downloaded via a browser.

Furthermore you can get the latest CI builds on
[GitHub's Actions page](https://github.com/ad-si/TaskLite/actions).
They are pretty stable if the build is successful due to the included tests.


### Linux

You can get the latest versions on
[GitHub's Releases page](https://github.com/ad-si/TaskLite/releases).

Furthermore you can get the latest CI builds on
[GitHub's Actions page](https://github.com/ad-si/TaskLite/actions).
They are pretty stable if the build is successful due to the included tests.


## Prebuilt Docker Image

Another easy way to get started is using the prebuilt Docker image:

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


## From Source

To build TaskLite from source, you need to [install Stack] first.

[install Stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/

```shell
git clone https://github.com/ad-si/TaskLite
cd TaskLite
make install
```

To test the installation run:

```shell
tasklite help
```
