# Changelog

This document lists all changes to the functionality of TaskLite.


## 2020-03-01 - <small>[0.3.0.0]</small>

[0.3.0.0]: https://github.com/ad-si/TaskLite/releases/tag/v0.3.0.0

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


## 2019-10-04 - <small>[0.2.2.0]</small>

[0.2.2.0]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.2.0

- Support optional filter expression after "count" command ([61e87b7])
- Automatically create a config file if it doesn't exist ([7407f87])


[61e87b7]: https://github.com/ad-si/TaskLite/commit/61e87b7
[7407f87]: https://github.com/ad-si/TaskLite/commit/7407f87


## 2019-07-14 - <small>[0.2.1.0]</small>

[0.2.1.0]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.1.0

- Fix creation of Docker image, extend documentation accordingly ([fa4cad3])

[fa4cad3]: https://github.com/ad-si/TaskLite/commit/fa4cad3


## 2019-06-13 - <small>[0.2.0.0]</small>

[0.2.0.0]: https://github.com/ad-si/TaskLite/releases/tag/v0.2.0.0

- Initial release
