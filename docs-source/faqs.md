# FAQs

<div id="faqs-page">

<style>
  #faqs-page summary {
    cursor: pointer;
    opacity: 0.6;
  }
  #faqs-page h2 {
    font-size: 1.1em;
    margin: 1em 0 0.2em 0;
  }
</style>


## Why another CLI task manager?

<details>
  <summary><small>Answer:</small></summary>

[Taskwarrior] has been the gold standard for CLI task managers so far.
However, I repeatedly lost tasks due to [weird bugs]
and syncing issues.
I also found several UI decisions inept and wanted something
with a better workflow.
But probably most importantly I couldn't see myself contributing
to a C++ project.
I had been working with C++ at university and it wasn't pleasant.

To sum it up: I finally wanted something which I could fully own
and use until the end of days.
That means:

- Does not suddenly get bought by a bigger fish and get closed down
    or made unusable (looking at you [Wunderlist])
- Is written in a high-performance programming language,
    yet gives me lot's of guarantees about the code's stability
    and makes it easy for other developers to contribute
- Free software
- With a stable, future proof, powerful, and fast backend
    (currently [SQLite], but support for plain files and Git is planned)

[Wunderlist]:
  https://www.theverge.com/2018/3/21/17146308/microsoft-wunderlist-to-do-app-acquisition-complicated
[SQLite]: https://sqlite.org
[Taskwarrior]: https://taskwarrior.org
[weird bugs]: https://github.com/GothenburgBitFactory/taskwarrior/issues/1831

</details>


## Why not Org-mode style?

<details>
  <summary><small>Answer:</small></summary>

I don't like [Org-mode]'s' unstructured combination
of outlining, notes and tasks.
Furthermore I don't like interactive document editing UIs in the terminal.
I prefer REPL style apps which adhere to UNIX conventions and let
me compose them easily with other CLI tools.

This, however, is just a personal preference
and otherwise [Org-mode] is certainly a good solution.
Also check out [Smos],
which is another powerful tree-based editor with extra focus
on [Getting Things Done].

[Org-mode]: https://orgmode.org/
[Smos]: https://smos.cs-syd.eu/
[Getting Things Done]: https://en.wikipedia.org/wiki/Getting_Things_Done

</details>


## What are your long term goals?

<details>
  <summary><small>Answer:</small></summary>

For the product roadmap check out the [dedicated page for it](/roadmap)

However, this project is not just about the product,
but just as well about the underlying values.
Big companies are good at offering you fancy products without
clarifying any of the adjacent issues which are crucial for
an outstanding user experience beyond the product itself:

- Will this service also be available in the future?
- What are your incentives to keep this service alive?
- What happens if the company dies?
- Can I export my data in a practical format?
- Who has data sovereignty?
- How do you share / sell my data?
- …

</details>


## Will this service also be available in the future?

<details>
  <summary><small>Answer:</small></summary>

[The code](https://github.com/ad-si/TaskLite)
is completely free open source software and compiling and using it
is straight forward.
Whatever happens to me, TaskLite will always be available in this way.

</details>


## What are your incentives to keep this service alive?

<details>
  <summary><small>Answer:</small></summary>

I'm using it daily and it has become something like my second brain.
Since I'm not interested in abandoning my brain,
it will be maintained and further developed in the future.

</details>


## What happens if you die?

<details>
  <summary><small>Answer:</small></summary>

A good friend has access to the repository and
can transfer it to the community.

</details>


## Can I export my data in a practical format?

<details>
  <summary><small>Answer:</small></summary>

Not locking you in is one of the most important aspects of TaskLite.
Therefore it supports numerous export formats:

- JSON - All tasks as one JSON array
- [JSON Lines](https://jsonlines.org) - One JSON object per task
- CSV
- SQL
- Direct access to the SQLite database

For more information check out the
[export documentation](/usage/cli.html#export)

</details>


## Who has data sovereignty?

<details>
  <summary><small>Answer:</small></summary>

All your data is stored in **your** TaskLite database on **your** computer.
No analytics data or data of any other kind gets transferred to a third party
during the usage of TaskLite.

</details>


## Do you share or sell my data?

<details>
  <summary><small>Answer:</small></summary>

We do not have access to any of your data!

</details>


## How do I keep up with any changes related to TaskLite?

<details>
  <summary><small>Answer:</small></summary>

- [Star TaskLite on GitHub](https://github.com/ad-si/TaskLite)
    to get notified about new releases in your GitHub feed
- Check out the [official changelog](/changelog.html)

</details>

</div>
