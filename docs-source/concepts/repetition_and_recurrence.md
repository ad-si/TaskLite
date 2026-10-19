# Repetition and Recurrence

The difference between repetition and recurrence is the reference point from
which future tasks are calculated from.

For repetition, it's the closing datetime.
E.g. if a task gets completed,
a duplicate will be created with the specified time offset added to
the closing datetime.
If the closed task had an awake or ready datetime,
their offsets to the due datetime are preserved.
Without an awake datetime, the new task sleeps until it's ready or due.
Check out the "Mow the law" example below for more details.

Recurring tasks are tasks which need to be done every day, week, etc. and
must not be delayed.
I.e. missed completions must be caught up immediately.
Check out the "Pay the rent" example below for more details.

A task is either recurring or repeating, but can't be both at the same time.
For both repetition and recurrence series, only the currently active task
exists and subsequent tasks will be created on demand,
once the current task is closed.


## Example for Repetition

```txt
$ tl add Mow the lawn awake:2020-09-24 due:2020-10-01
🆕 Added task "Mow the lawn" with id "01eme2w9pqbmgme5zcwr9hpfbc"

$ tl repeat P1M 01eme2w9pqbmgme5zcwr9hpfbc
📅 Set repeat duration of task "Mow the lawn"
with id "01eme2w9pqbmgme5zcwr9hpfbc" to "P1M"

$ tl info 01eme2w9pqbmgme5zcwr9hpfbc
Mow the lawn

   State: Open
Priority: 12.0
    ULID: 01eme2w9pqbmgme5zcwr9hpfbc

☀️   Awake    2020-09-24 00:00:00
       ⬇
📅    Due     2020-10-01 00:00:00
       ⬇
🆕  Created   2020-10-12 09:39:48
       ⬇
✏️   Modified  2020-10-12 09:39:58

Repetition Duration: P1M
Group Ulid: 01eme2wm3e4wcwacnw9ha7ffs1
User: adrian

$ tl do 01eme2w9pqbmgme5zcwr9hpfbc
✅ Finished task "Mow the lawn" with id "01eme2w9pqbmgme5zcwr9hpfbc"
➡️  Created next task "Mow the lawn"
in repetition series "01eme2wm3e4wcwacnw9ha7ffs1"
with id "01eme2xe4vjv3yd3gkg0a9y8j8"

$ tl info 01eme2xe4vjv3yd3gkg0a9y8j8
Mow the lawn

   State: Open
Priority: 3.0
    ULID: 01eme2xe4vjv3yd3gkg0a9y8j8

✏️   Modified  2020-10-12 09:39:58
       ⬇
🆕  Created   2020-10-12 09:40:25
       ⬇
☀️   Awake    2020-11-05 09:39:58
       ⬇
📅    Due     2020-11-12 09:39:58

Repetition Duration: P1M
Group Ulid: 01eme2wm3e4wcwacnw9ha7ffs1
User: adrian
```


## Example for Recurrence

```txt
$ tl add Pay rent due:2020-10-01
🆕 Added task "Pay rent" with id "01eme47dje7bpkmz01s5xdtw15"

$ tl recur P1M 01eme47dje7bpkmz01s5xdtw15
📅 Set recurrence duration of task "Pay rent"
with id "01eme47dje7bpkmz01s5xdtw15" to "P1M"

$ tl info 01eme47dje7bpkmz01s5xdtw15
Pay rent

   State: Open
Priority: 12.0
    ULID: 01eme47dje7bpkmz01s5xdtw15

📅    Due     2020-10-01 00:00:00
       ⬇
🆕  Created   2020-10-12 10:03:21
       ⬇
✏️   Modified  2020-10-12 10:03:32

Recurrence Duration: P1M
Group Ulid: 01eme47s0yy848wvbsyxh9mpj6
User: adrian

$ tl do 01eme47dje7bpkmz01s5xdtw15
✅ Finished task "Pay rent" with id "01eme47dje7bpkmz01s5xdtw15"
➡️  Created next task "Pay rent"
in recurrence series "01eme47s0yy848wvbsyxh9mpj6"
with id "01eme487qmxj7jm4mtn5n59nbg"

$ tl info 01eme487qmxj7jm4mtn5n59nbg
Pay rent

   State: Open
Priority: 3.0
    ULID: 01eme487qmxj7jm4mtn5n59nbg

✏️   Modified  2020-10-12 10:03:32
       ⬇
🆕  Created   2020-10-12 10:03:47
       ⬇
📅    Due     2020-11-01 00:00:00

Recurrence Duration: P1M
Group Ulid: 01eme47s0yy848wvbsyxh9mpj6
User: adrian
```
