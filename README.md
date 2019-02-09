# TaskLite

CLI task-list manager built with [Haskell] and [SQLite].

<img
  src='screenshots/withtag.svg'
  alt="Screenshot of all TaskLite related tasks in the maintainer's database"
  width='600'
/>


## Installation

```shell
stack install tasklite
```

or

```shell
docker pull adius/tasklite
```


## Usage

To run TaskLite in a container:

```sh
docker run --rm --volume ~/tasklite:/root/tasklite adius/tasklite
```


### Web Interface

The web interface can currently only be used to display existing tasks,
but not to create new ones.

![Web User Interface](screenshots/web-ui.png)

Use [Datasette] to serve the web frontend for the SQLite database:

```sh
docker run \
  --rm \
  --entrypoint datasette \
  --publish 8001:8001 \
  --volume ~/tasklite:/root/tasklite \
  --volume web:/root/web \
  adius/tasklite-tasklite \
  serve \
    --host 0.0.0.0 \
    /root/tasklite/main.db
```

Attention: Make sure that the IP address matches with your host's.

Generate custom view by appending the SQL query to [0.0.0.0:8001/main?sql=]
(e.g. [0.0.0.0:8001/main?sql=select%20*%20from%20tasks])
<!-- Fixes syntax highlighting -->
[0.0.0.0:8001/main?sql=]: http://0.0.0.0:8001/main?sql=
[0.0.0.0:8001/main?sql=select%20*%20from%20tasks]:
  http://0.0.0.0:8001/main?sql=select%20*%20from%20tasks

Some example views:

Equivalent to `tl head`:
```sql
select substr(ulid,22) as ulid,priority,body,due_utc,replace(tags,',',', ') as tags,notes,user %20
from tasks_view %20
where closed_utc is null %20
order by priority desc %20
limit 50
```

(`%20` is necessary to make it copy & pasteable):


Make sure to bookmark the views for easy access.


## Differences to Taskwarrior

- **Simpler** \
  Taskwarrior has several redundant features and unnecessarily re-implements
  shell features like aliases.

- **More Robust & Stable** \
  Taskwarrior is plagued by [numerous bugs][TW Issues] due to its
  unnecessary complexity and non-optimal choice of programming languages.
  TaskLite's simple structure and [Haskell]'s excellent correctness guarantees,
  however, yield to a very stable and robust piece of software.

- **Faster** \
  [Haskell] plus [SQLite] yields to an outstanding performance.

- **More Powerful** \
  As all tasks are stored in a [SQLite] database, you can use all the available
  tooling for it to supercharge your TaskLite installation.
  For example [Datasette] for an instant REST API, or [DB Browser for SQLite]
  to manipulate and view your data in a GUI.

[TW Issues]: https://github.com/GothenburgBitFactory/taskwarrior/issues
[Datasette]: https://github.com/simonw/datasette
[DB Browser for SQLite]: https://sqlitebrowser.org


## Manual

### Help

```shell
tasklite help
```

<img
  src='screenshots/recording.svg'
  alt='Screenshot of TaskLite help page'
  width='600'
/>


### Add

To add a task run

```shell
tasklite add Improve the TaskLite manual
```

It is also possible to immediately add tags when creating a task:

```shell
tasklite add Improve the TaskLite manual +tasklite +pc
```

(Attention: The tags must be the last parameters)


### Context / Views

For example I have following `work` command in my `$PATH`:

```bash
#! /usr/bin/env bash

tasklite query \
  "(tags is null or tags not like '%feram%') \
    and state is 'Open' \
    order by priority \
    desc limit 10"
```


### Import / Export

Import a GitHub issue:

```
curl https://api.github.com/repos/$OWNER/$REPO/issues/$NUM | tl import
```


### States

Instead of allowing one to explicitly set a state, TaskLite infers the
current state from several other fields.

Each task can only be in one of the 10 exclusive main states at any given time.

- Open - Waits to be done
- Asleep - Is hidden because it's not relevant yet
- Awake - Has become relevant or will become soon
- Ready - Is ready to be done (similar to Open)
- Waiting - It's still unclear if the task needs to be done or really has been
    done. Regular checks are necessary until situation clears up.
- Review - It's necessary to check if the task can finally be started or
    if it has finally been completed.
- Done - Has been done
- Obsolete - Has become obsolete or impossible to finish
- Deletable - Not needed anymore and can be deleted
- Blocked - Some other task(s) must be done first.
    Blockers are stored in a separate table.


State\Field| state   | awake_utc| ready_utc| waiting_utc| review_utc| closed_utc
-----------|---------|----------|----------|------------|-----------|-----------
Open       | ❌      | ❌       | ❌      |     ❌     | ❌        | ❌
Asleep     | ❌      | > now    | > now    |     ❌     | ❌       | ❌
Awake      | ❌      | < now    | > now    |     ❌     | ❌       | ❌
Ready      | ❌      | > now    | > now    |     ❌     | ❌       | ❌
Waiting    | ❌      |   ❔     |   ❔    |     ✅     | > now     | ❌
Review     | ❌      |   ❔     |   ❔    |     ✅     | < now     | ❌
Done       | Done     |   ❔    |   ❔     |     ❔     | ❔       | ❔
Obsolete   | Obsolete |   ❔    |   ❔     |     ❔     | ❔       | ❔
Deletable  | Deletable|   ❔    |   ❔     |     ❔     | ❔       | ❔
Blocked    | ❌      |    ❔    |   ❔     |     ❔     | ❔       | ❌

Legend:
- ❌ = Not allowed
- ✅ = Required
- ❔ = Maybe


Additional secondary states:

- Repeating - If this task get completed, a duplicate will be created
    with the specified time offset.
    I.e. subsequent tasks get delayed
    (e.g. mowing the lawn)
- Recurring - Task which needs to be done every day, week, etc.
    I.e. missed completions must be caught up immediately.
    (e.g. paying rent)
    The number of tasks which will be created in advance
    can be set via a config.


State\Field | group_ulid | repetition_duration | recurrence_duration
------------|------------|---------------------|---------------------
Repeating   | ✅         | ✅                  | ❌
Recurring   | ✅         | ❌                  | ✅

\* implemented with an additional table


### REST API

Powered by [Datasette]:

```shell
datasette serve ~/tasklite/main.db
```

```shell
curl --location http://127.0.0.1:8001/main/tasks_view.json
```


## Development

### Process

Ghcid with color output for GHC 8.4

```sh
ghcid \
  --command="stack ghci --ghci-options=-fdiagnostics-color=always"
```

```sh
hlint \
  --ignore="Redundant do" \
  --ignore="Use list literal" \
  --ignore="Use String" \
  --ignore="Redundant bracket" \
  --ignore="Use camelCase" \
  .
```


### Build Images

Build base image for runtime image:

```sh
docker build \
  --file dockerfiles/haskell-datasette \
  --tag haskell-datasette \
  dockerfiles
```

Build runtime image:

```sh
stack image container
docker tag adius/tasklite-tasklite:latest adius/tasklite:latest
```


### Deployment

```sh
docker tag adius/tasklite-tasklite:latest gcr.io/deploy-219812/tasklite:latest
```

```sh
docker push gcr.io/deploy-219812/tasklite:latest
```

```sh
kubectl create -f kubernetes/deployment.yaml
```

```sh
kubectl port-forward tasklite-deployment-77884ff4f6-66sjf 8001
```

Open [127.0.0.1:8001](http://127.0.0.1:8001)


```fish
docker build \
  --file dockerfiles/nginx-proxy \
  --tag gcr.io/deploy-219812/nginx-proxy:latest \
  dockerfiles; \
and docker push gcr.io/deploy-219812/nginx-proxy:latest; \
and kubectl replace --filename kubernetes/deployment.yaml --force; \
and sleep 8;
and kubectl port-forward \
  (kubectl get pods --selector app=tasklite --output name) 8080
```

Afterwards change the health check URL to `/healthcheck`
for the load balancer at https://console.cloud.google.com/compute/healthChecks.


### Generate Screenshot

Use asciinema to generate the terminal recording:

```sh
asciinema rec \
  --title 'TaskLite Help Page' \
  --command 'tasklite help' \
  --overwrite \
  screenshots/recording.json
```

```sh
asciinema rec \
  --title 'TaskLite "withtag" Command' \
  --command 'tasklite withtag tasklite' \
  --overwrite \
  screenshots/withtag.json
```

Change the size of the terminal in the recording.json file to:

```json
  "width": 80,
  "height": 86,
```

Then use [svg-term] to generate the SVG image:

```sh
svg-term \
  --no-cursor \
  --at 99999 \
  --window \
  --term iterm2 \
  --profile ../../dotfiles/terminal/adius.itermcolors \
  < screenshots/recording.json \
  > screenshots/recording.svg
```

[svg-term](https://github.com/marionebl/svg-term-cli)


## Programmatic Use

While TaskLite is great tool to manage your personal tasks,
it can also be used as a dependency of other programms.
For example as a queue for processing tasks.


## 3rd Party Tools to Edit Tables

- [VisiData] - Interactive CLI multitool for tabular data.
- [DB Browser for SQLite]
- [LiteCLI] - CLI browser with auto-completion and syntax highlighting.

[VisiData]: http://visidata.org
[LiteCLI]: https://litecli.com


## Related

- [Buku] -Store and manage your bookmarks from the command line
- [CommitTasks]
- [Eureka] - CLI tool to input and store ideas without leaving the terminal
- [Ff] - A distributed note taker and task manager.
- [Taskbook]
- [Taskwarrior] - Commandline Task Management.
- [Toodles] - Project management from the TODO's in your codebase

[Buku]: https://github.com/jarun/Buku
[CommitTasks]: https://github.com/ZeroX-DG/CommitTasks
[Eureka]: https://github.com/simeg/eureka
[Ff]: https://github.com/ff-notes/ff
[Taskbook]: https://github.com/klauscfhq/taskbook
[Taskwarrior]: https://github.com/GothenburgBitFactory/taskwarrior
[Toodles]: https://github.com/aviaviavi/toodles


[Haskell]: https://haskell.org
[SQLite]: https://sqlite.org
