
# States

Instead of allowing one to explicitly set a state, TaskLite infers the
current state from several other fields.
There are primary, secondary, and tertiary states.

**2 primary states:**

- `Open` - Waits to be done
- `Closed` - Nothing left to be done

**8 secondary states:**

They are not fully exclusive, as a task that should be reviewed is also ready.
The info command, however, only shows the most relevant one.

- `Asleep` - Is hidden because it's not relevant yet. \
    E.g. the parcel can only be sent after `awake_utc`.
- `Awake` - Has become relevant or will become soon. \
    E.g. the parcel could be sent now.
- `Ready` - Is now ready to be done and should be started as soon as possible. \
    E.g. the parcel should be sent now.
- `Waiting` -
    It is unclear if the task really has been done.
    Regular checks are necessary until situation clears up. \
    E.g. the parcel was sent and waiting for confirmation of receipt.
- `Review` - It's necessary to check if the task can finally be started or
    if it has finally been completed. \
    E.g. investigate if the parcel has been sent or received.
- `Done` - Has been done.
- `Obsolete` - Has become obsolete or impossible to finish.
- `Deletable` - Not needed anymore and can be deleted (item in the trash).

<!--
TODO: Add Blocked
- `Blocked` - Some other task(s) must be done first.
    Blockers are stored in a separate table.
Table row:
`└─Blocked`  |   ❔      |    ❔    |    ❔    |    ❔     |  ❌  |    ❌
-->

&nbsp;

<small>

State\Field|`awake_utc`|`ready_utc`|`waiting_utc`|`review_utc`|`closed_utc`|`state`
-----------|:---------:|:---------:|:----------:|:---------:|:--------:|:------:
**`Open`**   |   ❔       |    ❔      |   ❔    |    ❔     |  ❌   |    ❌
`└─Asleep`   | > now      | > now or ❌|   ❌    |    ❌     |  ❌   |    ❌
`└─Awake`    | < now      | > now or ❌|   ❌    |    ❌     |  ❌   |    ❌
`└─Ready`    | < now or ❌| < now      |   ❌    | < now or ❌|  ❌  |    ❌
`└─Waiting`  |   ❔       |    ❔      |   < now | > now or ❌|  ❌  |    ❌
`└─Review`   |   ❔       |    ❔      |    ❔    | < now     |  ❌  |    ❌
**`Closed`** |   ❔       |    ❔      |    ❔    |    ❔     |  ✅  |    ❔
`└─Done`     |   ❔       |    ❔      |    ❔    |    ❔     |  ✅  |`Done`
`└─Obsolete` |   ❔       |    ❔      |    ❔    |    ❔     |  ✅  |`Obsolete`
`└─Deletable`|   ❔       |    ❔      |    ❔    |    ❔     |  ✅  |`Deletable`

</small>

Legend:
- ✅ = Set
- ❌ = Not set
- ❔ = Maybe set


**3 exclusive tertiary states:**

- `Repeating` - If this task gets completed, a duplicate will be created
    with the specified time offset.
    I.e. subsequent tasks get delayed
    (e.g. mowing the lawn)
- `Recurring` - Task which needs to be done every day, week, etc.
    I.e. missed completions must be caught up immediately.
    (e.g. paying rent)
    The number of tasks which will be created in advance
    can be set via a config.
- `Frozen` - Was previously repeating or recurring but has been stopped
    for the time being.


State\Field |`group_ulid`|`repetition_duration`|`recurrence_duration`
------------|------------|---------------------|---------------------
`Repeating` | ✅         | ✅                  | ❌
`Recurring` | ✅         | ❌                  | ✅
`Frozen`    | ✅         | ❌                  | ❌

A task is either recurring or repeating,
but can't be both at the same time.
For more information and examples check out the
[corresponding documentation page](repetition_and_recurrence.html)
