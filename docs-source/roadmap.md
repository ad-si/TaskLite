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


## More Features for Using It as a Personal Knowledge Base

- [ ] Tasks can be a note
- [ ] Tasks can be linked to each other
  - [ ] Visualize the graph of linked tasks
- [ ] Export to markdown files a la Zettelkasten apps like Obsidian
- [ ] Export to PDF, Anki, etc.


## Syncing

There is no concrete plan for native synchronization support yet.
I've been using Dropbox for synchronizing the TaskLite database
among several computers, which has worked astonishingly well so far.

Another good solution could be [Litestream](https://litestream.io),
which continuously streams SQLite changes to S3-compatible storage.


## Desktop App

While there is already a prototype for a desktop app included in the code,
I currently don't plan to further work on it.
The integrated webapp is sufficient for most use cases.
However, I'm open to contributions!


## Mobile App

There are no plans for a native mobile app.
The integrated webapp is sufficient for most use cases.
