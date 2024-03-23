# Roadmap

---
<!-- toc -->
---

There is no time frame when the following enhancements will be implemented
since the amount of time I can spare to work on TaskLite fluctuates heavily.

However, I'm using it daily and it has become something like my second brain.
Since I'm not interested in abandoning my brain, rest assured that there will
be steady progress on this in the future.

The enhancements are roughly listed with descending priority.


## Feature Parity with Taskwarrior

Since TaskLite started out as a replacement for Taskwarrior,
one of my main goals is to reach feature parity with it.
Some of the major features still missing:

- [ ] Hooks
- [ ] History View
- [ ] Burndown Chart
- [ ] Calendar View

Check out the page "[Differences to Taskwarrior](/differences_taskwarrior)"
for a detailed comparison.


## Tasklite Core

Currently the core of TaskLite and the CLI are somewhat mixed up in the code.
I'd like to split them up, so that TaskLite Core can be used
as a library in other applications
which need to manage internal tasks / queues.

Check out the page
[Haskell API for Programmatic Usage](https://tasklite.org/usage/haskell_api)
for more information.


## Syncing

There is no concrete plan for native synchronization support yet.
I've been using Dropbox for synchronizing the TaskLite database
among several computers, which has worked astonishingly well so far.

Another good solution could be [Litestream](https://litestream.io),
which continuously streams SQLite changes to S3-compatible storage.


## Apps

### Webapp

I'd like to be able to run TaskLite on a cheap v-server in a dedicated
server mode and have it host a simple REST API,
and an [Elm](https://elm-lang.org) webapp consuming it.

This would then be a good way to casually browse and edit the tasks
in a more graphical way.


### Mobile App

There are no plans for a native mobile app.
The webapp should be sufficient for most use cases.


### Desktop App

While there is already a prototype for a desktop app included in the code,
I currently don't plan to further work on it any time soon.
A webapp -- which could also be hosted locally --
should be sufficient for most use cases.
