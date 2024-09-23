# Changelog

This document lists all notable changes to the functionality of TaskLite.

---
<!-- toc -->
---


## 2024-09-23 - <small>[0.4]</small>

[0.4]: https://github.com/ad-si/TaskLite/releases/tag/v0.4.0.0


#### General

- Add simple webapp powered by [elm.land](https://elm.land)
- Add support for hooks to execute code during a task's lifecycle ([ba2054b])
- Set text color according to current background color ([1d9ae64])
- Add support for recurring tasks ([8028190])
- Add support for external CLI commands ([9cb8fa3])
- Use `.yaml` extension for `edit` to have syntax highlighting ([10e1e52])
- Support the full ISO8601 duration syntax everywhere ([8df1c65])
- Add support for filter expressions to sub-command `new` ([9ab3992])
- Improve design of detail view (used by "info" & "next" commands) ([c32be9f])
- Extend and restructure the documentation
- Webapp: Show selected tags in header ([ff1bf2e])
- Edit mode: If parsing fails, re-open editor with modified content ([2b5a0e1])
- Improve import of user, title, labels fields
    (needed for importing GitHub issues) ([af51aaa])


#### New Subcommands

- `stats` - List share of Done, Obsolete, Deletable tasks ([c2a2d70])
- `server` - Starts an API + GraphQL server powered by [AirGQL] ([4669498])
- `random` - Show a random task ([88a999d])
- `deletetag` ([d3ab4de])
- `deletenote` ([581ffae])
- `json` - Show tasks in JSON format ([45ad3f3])
- `reviewin` - Set review date in x days ([b05c81a])
- `idea` - Quickly capture ideas ([6545500])
- `modified` - List all tasks by modified UTC desc ([71e4603])
- `modifiedonly` - List tasks where modified UTC /= creation UTC
    by modified UTC desc ([4809d5b])
- `notes` - List all notes descending by creation UTC ([81e6885])
- `recurring` - List recurring tasks ([0c11309])
- `ingest` - Runs `import` -> `edit` -> `delete` workflow on files ([325e0ac])
- `importdir` - Import all .json and .eml files in a directory ([4c3c50a])
- `ingestdir` - Ingest all .json and .eml files in a directory ([4c3c50a])
- `endall` - End all provided tasks
    (change `end` to accept a closing note) ([4d2032e])
- `unrecur` - Remove recurring timestamp ([2ee1c21])
- `unwake` - Remove awake timestamp ([987a4fe])
- `unready` - Remove ready timestamp ([987a4fe])
- `unreview` - Remove review timestamp ([1d82631])

[AirGQL]: https://github.com/Airsequel/AirGQL


#### Import

- Add support for importing `.json` and `.eml` files ([8a821de])
- Support list of strings as `notes` in JSON import ([d07a328])
- Add support for parsing unix timestamps with 10, 13, and 16 digits ([ce0392d])
- Add support for milliseconds in UTC timestamps ([2e580ba])
- Use timestamp of parent task for imported tags and notes ([ac15d47])
- Make it more robust (works with a larger variety of files now)

Check out the [commit history] for all changes.

[commit history]: https://github.com/ad-si/TaskLite/commits/master/


## 2020-03-01 - <small>[0.3]</small>

[0.3]: https://github.com/ad-si/TaskLite/releases/tag/v0.3.0.0

- Add `edit` command to edit YAML version of task in `$EDITOR` ([1add89e])
- Add several `un*` commands to erase fields ([0f09c3d])
- Only execute trigger to set `closed_utc` after state changed ([395a8e0])
- Show descriptive variable names in brief help text ([10f8cf6])
- Hide aliases from main help ([c52df72])
- Display alias errors even with subarguments ([c52df72])
- Fix parsing of timestamp part in ULIDs for small timestamp values ([258df47])
- Also display full version slug with `tl version` ([0c292d1])
- Remove unnecessary import logging ([c04b894])
- Add git hash to the version string ([5f7b1ef])
- Create config directory if it does not exist ([8d0657c])

[1add89e]: https://github.com/ad-si/TaskLite/commit/1add89e
[0f09c3d]: https://github.com/ad-si/TaskLite/commit/0f09c3d
[395a8e0]: https://github.com/ad-si/TaskLite/commit/395a8e0
[10f8cf6]: https://github.com/ad-si/TaskLite/commit/10f8cf6
[c52df72]: https://github.com/ad-si/TaskLite/commit/c52df72
[c52df72]: https://github.com/ad-si/TaskLite/commit/c52df72
[258df47]: https://github.com/ad-si/TaskLite/commit/258df47
[0c292d1]: https://github.com/ad-si/TaskLite/commit/0c292d1
[c04b894]: https://github.com/ad-si/TaskLite/commit/c04b894
[5f7b1ef]: https://github.com/ad-si/TaskLite/commit/5f7b1ef
[8d0657c]: https://github.com/ad-si/TaskLite/commit/8d0657c


## 2019-10-04 - <small>[0.2.2]</small>

[0.2.2]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.2.0

- Support optional filter expression after "count" command ([61e87b7])
- Automatically create a config file if it doesn't exist ([7407f87])


[61e87b7]: https://github.com/ad-si/TaskLite/commit/61e87b7
[7407f87]: https://github.com/ad-si/TaskLite/commit/7407f87


## 2019-07-14 - <small>[0.2.1]</small>

[0.2.1]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.1.0

- Fix creation of Docker image, extend documentation accordingly ([fa4cad3])

[fa4cad3]: https://github.com/ad-si/TaskLite/commit/fa4cad3


## 2019-06-13 - <small>[0.2]</small>

[0.2]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.0.0

- Initial release
