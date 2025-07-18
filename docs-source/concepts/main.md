# Concepts

---
<!-- toc -->
---


## General

TaskLite tries to adhere to the Unix philosophy:
*Do one thing and do it well.*
This, however, also means that it intentionally
does not include certain features, which you might expect it to have.

For example, there is no support for fuzzy timestamps like "in 2 weeks",
as this should be handled by other CLI tools.
Since CLI tools can be easily composed, this is no disadvantage!

For example, I use [`tu`](https://github.com/ad-si/tu) to convert
such fuzzy times to exact timestamps:

```sh
tl add Buy milk due:$(tu in 2 weeks)
```

And the big advantage of composing tools is,
that you now can use `tu` in other contexts as well!

The power of the Unix philosophy: \
**Each new tool you learn gives you a compound interest on all other tools!**

> [!TIP]
> If you use [`fish`](https://fishshell.com/), it will be even shorter:
> ```fish
> tl add Buy milk due:(tu in 2 weeks)
> ```


## Priority

The priority of a task is a decimal number
between negative and positive infinity.
It is automatically calculated based on the age, the due date,
and several other values.

![Priority](images/priority.png)

The idea is that you never have to manually set a priority,
because it can be derived accurately from other values.
This of course requires you
to use the other available meta information adequately!

The exact calculation algorithm can be found
in the SQL view `tasks_view` in the [Migrations.hs] file.

[DbSetup.hs]:
  https://github.com/ad-si/TaskLite/blob/master/tasklite-core/source/Migrations.hs

If you want to adjust the priority of selected tasks manually,
you can use the `tl boost [ulid]` command to increase the priority by 1,
or the `tl hush [ulid]` command to decrease it by 1.


## Notes

A task can have several notes. Each note is identified by an [ULID].

[ULID]: https://github.com/ulid/spec

```txt
$ tl add Buy milk
🆕 Added task "Buy milk" with id "01dpgj8e9ws2dwgvsk5nmrvvg9"

$ tl note 'The vegan one from Super Buy' 01dpgj8e9ws2dwgvsk5nmrvvg9
🗒  Added a note to task "Buy milk" with id "01dpgj8e9ws2dwgvsk5nmrvvg9"

$ tl info 01dpgj8e9ws2dwgvsk5nmrvvg9
awake_utc: null
review_utc: null
state: null
repetition_duration: null
recurrence_duration: null
body: Buy milk
user: adrian
ulid: 01dpgj8e9ws2dwgvsk5nmrvvg9
modified_utc: 2019-10-06 12:59:46
group_ulid: null
closed_utc: null
priority_adjustment: null
metadata: null
waiting_utc: null
ready_utc: null
due_utc: null
priority: 1.0
tags:

notes:
  - note: The vegan one from Super Buy
    ulid: 01dpgjf35pq74gchsgtcd6fgsa
```
