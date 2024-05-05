# Desktop App

The desktop app can currently only list the tasks.
It's implemented with a [declarative Haskell wrapper][gi-gtk-declarative]
for GTK.

[gi-gtk-declarative]: https://github.com/owickstrom/gi-gtk-declarative

![Screenshot of desktop app](
  ../images/screenshots/desktop_app/2019-06-13_shadow.png)


## Installation

You can install the desktop app using
[Stack](https://docs.haskellstack.org/en/stable/):

```sh
git clone https://github.com/ad-si/TaskLite
cd archive/tasklite-app
stack install
```
