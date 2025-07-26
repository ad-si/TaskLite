# Changelog

This document lists all notable changes to the functionality of TaskLite.

---
<!-- toc -->
---

## 2025-07-26 - [0.5]

[0.5]: https://github.com/ad-si/TaskLite/releases/tag/v0.5.0.0


### 💻 General

- Add new subcommand `enter` to open the default editor with an empty task ([09df546](https://github.com/ad-si/TaskLite/commit/09df5464991e7541d3b6b7b6cfd6f6d3213563ba))
- Add support for importing YAML files ([e5287d2](https://github.com/ad-si/TaskLite/commit/e5287d2a8d6ba6dd3ef7401f122c95f663f855fa))
    and Markdown files ([87d5ae5](https://github.com/ad-si/TaskLite/commit/87d5ae55362cb1a57d91331251fc2ac4db6b7666))
- Use Markdown with frontmatter instead of YAML for editing tasks ([a22079d](https://github.com/ad-si/TaskLite/commit/a22079dd476e265d09513d3e835546fde290ef67))
- Add config field to select visible table columns and their order ([9cbdaf5](https://github.com/ad-si/TaskLite/commit/9cbdaf5eeabee039b974364493a6da1127164fd4))
- Add support for new `Age` column that shows the age of a task in a human readable fuzzy way ([9cbdaf5](https://github.com/ad-si/TaskLite/commit/9cbdaf5eeabee039b974364493a6da1127164fd4))
- Extend `random` subcommand to accept an optional filter expression ([3d89593](https://github.com/ad-si/TaskLite/commit/3d895934c43404a15cd8532e32d774776d44bdfb))
- Support filters with `open` command, `get` returns any state again ([3be80d2](https://github.com/ad-si/TaskLite/commit/3be80d273edf39ecd927b930cd275deaec023f3f))
- Improve results of fuzzy search ([b9fcbe1](https://github.com/ad-si/TaskLite/commit/b9fcbe106a4448e73b7770df7f9d008d5df96146))
    - Score continuous matches higher
    - Weight body score higher
    - Increase necessary minimum score
    - Always include the body in result output
- Adapt the printed tasks count to the available terminal lines count ([c3c740d](https://github.com/ad-si/TaskLite/commit/c3c740d216c95b7d001a4981c9cab11ab4ad08ee))
- Only show first line of multi-line tasks in task listings ([2b9249a](https://github.com/ad-si/TaskLite/commit/2b9249ac7ae1658ed83b44f891dc65132345aa9f))
- Add support for `--no-color` flag and `NO_COLOR` env variable ([6d91299](https://github.com/ad-si/TaskLite/commit/6d91299656a44e9018c90f71496e01f531ee71d0))
- Add ARM builds to GitHub Action artifacts for macOS ([2c88eb7](https://github.com/ad-si/TaskLite/commit/2c88eb7cb399d7a247718dd6549ecd8b12ae4072))
    and for Linux ([b6a814f](https://github.com/ad-si/TaskLite/commit/b6a814fd2a1826bd46fc5eea885ef2d19ba4157f))


### 🩹 Fixes

- Correctly apply the `maxWidth` setting from the config ([ce8dddd](https://github.com/ad-si/TaskLite/commit/ce8dddd2644159f71f19c28e8c5b3eebd1d78ba3))
- Fix execution of pre-modify hook ([340d3a2](https://github.com/ad-si/TaskLite/commit/340d3a28bfc58e136fc3146337c3b8423ae60bb6))
- Show warning if there are no tags or projects defined yet ([02448be](https://github.com/ad-si/TaskLite/commit/02448be403caae619a688d9192048b96ad9459a6))
- Include notes when exporting tasks as ndjson ([2bd11a1](https://github.com/ad-si/TaskLite/commit/2bd11a1e490083103fe0cf225e73ca0968c2de57))
- Correctly handle recurring tasks without a due date ([7ddddf5](https://github.com/ad-si/TaskLite/commit/7ddddf57cf7cfc87c920fd9921651456f10eacaa))
- Make state matching in filter expressions case insensitive ([9f4cf31](https://github.com/ad-si/TaskLite/commit/9f4cf3164eb09affdf81fd7916fb6f7d476351a5))
- Update `ready` state to match documentation. ([2935a99](https://github.com/ad-si/TaskLite/commit/2935a99c5c4c0e6e82d33741b361ac2c1d1817a7))


### 🌐 Webapp

- Set correct state when closing a task ([2f3febc](https://github.com/ad-si/TaskLite/commit/2f3febcdf290038419893c235f78f3bfc6c24770))


### 📚 Documentation

- Add documentation on how to list and filter tasks ([506d29e](https://github.com/ad-si/TaskLite/commit/506d29e8ed4290862eec4706c06cf56587c2dfaf))
- Explain handling of time and date ([283471a](https://github.com/ad-si/TaskLite/commit/283471a8d86d2bca97dce382b2528f477a55a1de))
- Fix documentation for `query` command ([081d3a7](https://github.com/ad-si/TaskLite/commit/081d3a7097732a86357d6c02dd9010aa66da3d46))
- Fix installation instructions ([d7ceb39](https://github.com/ad-si/TaskLite/commit/d7ceb399193a8a520e9383fec21f426ae71be0dd))
- Add note taking apps to related page ([5d0eace](https://github.com/ad-si/TaskLite/commit/5d0eaceab006079c3454c31fb9c6d501cfabe712))
- Add taskfinder ([cea4447](https://github.com/ad-si/TaskLite/commit/cea44470a5489df78e1c676845e726a58efc49ca))
    and Topydo to related.md ([3de5f8e](https://github.com/ad-si/TaskLite/commit/3de5f8ed831548e850a79477ab7d3b816b68955d))


## 2024-09-23 - [0.4]

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


[ba2054b]: https://github.com/ad-si/TaskLite/commit/ba2054b
[1d9ae64]: https://github.com/ad-si/TaskLite/commit/1d9ae64
[8028190]: https://github.com/ad-si/TaskLite/commit/8028190
[9cb8fa3]: https://github.com/ad-si/TaskLite/commit/9cb8fa3
[10e1e52]: https://github.com/ad-si/TaskLite/commit/10e1e52
[8df1c65]: https://github.com/ad-si/TaskLite/commit/8df1c65
[9ab3992]: https://github.com/ad-si/TaskLite/commit/9ab3992
[c32be9f]: https://github.com/ad-si/TaskLite/commit/c32be9f
[ff1bf2e]: https://github.com/ad-si/TaskLite/commit/ff1bf2e
[2b5a0e1]: https://github.com/ad-si/TaskLite/commit/2b5a0e1
[af51aaa]: https://github.com/ad-si/TaskLite/commit/af51aaa
[c2a2d70]: https://github.com/ad-si/TaskLite/commit/c2a2d70
[4669498]: https://github.com/ad-si/TaskLite/commit/4669498
[88a999d]: https://github.com/ad-si/TaskLite/commit/88a999d
[d3ab4de]: https://github.com/ad-si/TaskLite/commit/d3ab4de
[581ffae]: https://github.com/ad-si/TaskLite/commit/581ffae
[45ad3f3]: https://github.com/ad-si/TaskLite/commit/45ad3f3
[b05c81a]: https://github.com/ad-si/TaskLite/commit/b05c81a
[6545500]: https://github.com/ad-si/TaskLite/commit/6545500
[71e4603]: https://github.com/ad-si/TaskLite/commit/71e4603
[4809d5b]: https://github.com/ad-si/TaskLite/commit/4809d5b
[81e6885]: https://github.com/ad-si/TaskLite/commit/81e6885
[0c11309]: https://github.com/ad-si/TaskLite/commit/0c11309
[325e0ac]: https://github.com/ad-si/TaskLite/commit/325e0ac
[4c3c50a]: https://github.com/ad-si/TaskLite/commit/4c3c50a
[4c3c50a]: https://github.com/ad-si/TaskLite/commit/4c3c50a
[4d2032e]: https://github.com/ad-si/TaskLite/commit/4d2032e
[2ee1c21]: https://github.com/ad-si/TaskLite/commit/2ee1c21
[987a4fe]: https://github.com/ad-si/TaskLite/commit/987a4fe
[987a4fe]: https://github.com/ad-si/TaskLite/commit/987a4fe
[1d82631]: https://github.com/ad-si/TaskLite/commit/1d82631
[8a821de]: https://github.com/ad-si/TaskLite/commit/8a821de
[d07a328]: https://github.com/ad-si/TaskLite/commit/d07a328
[ce0392d]: https://github.com/ad-si/TaskLite/commit/ce0392d
[2e580ba]: https://github.com/ad-si/TaskLite/commit/2e580ba
[ac15d47]: https://github.com/ad-si/TaskLite/commit/ac15d47


## 2020-03-01 - [0.3]

[0.3]: https://github.com/ad-si/TaskLite/releases/tag/v0.3.0.0

- Add `edit` command to edit YAML version of task in `$EDITOR` ([1add89e])
- Add several `un*` commands to erase fields ([0f09c3d])
- Only execute trigger to set `closed_utc` after state changed ([395a8e0])
- Show descriptive variable names in brief help text ([10f8cf6])
- Hide aliases from main help ([c52df72])
- Display alias errors even with sub-arguments ([c52df72])
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


## 2019-10-04 - [0.2.2]

[0.2.2]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.2.0

- Support optional filter expression after "count" command ([61e87b7])
- Automatically create a config file if it doesn't exist ([7407f87])


[61e87b7]: https://github.com/ad-si/TaskLite/commit/61e87b7
[7407f87]: https://github.com/ad-si/TaskLite/commit/7407f87


## 2019-07-14 - [0.2.1]

[0.2.1]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.1.0

- Fix creation of Docker image, extend documentation accordingly ([fa4cad3])

[fa4cad3]: https://github.com/ad-si/TaskLite/commit/fa4cad3


## 2019-06-13 - [0.2]

[0.2]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.0.0

- Initial release
