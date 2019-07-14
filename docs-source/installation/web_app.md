# Web App

The web app is currently based on [Datasette] and can
only be used to view tasks, but not to create new ones.

In combination with the Docker container
the web frontend for the SQLite database can be served in following way:

[Datasette]: https://github.com/simonw/datasette

```sh
docker run \
  --rm \
  --entrypoint datasette \
  --publish 8001:8001 \
  --volume ~/TaskLite:/root/tasklite \
  --volume "$PWD"/datasette:/root/datasette \
  adius/tasklite \
  serve \
    --host 0.0.0.0 \
    --metadata /root/datasette/metadata.json \
    --reload \
    /root/tasklite/main.db
```

**Attention:** Make sure that the IP address matches with your host's.

There is a predefined query for a `tl head` like overview:
<http://0.0.0.0:8001/main/tasks_pretty>

Generate custom view by appending the SQL query to
<http://0.0.0.0:8001/main?sql=>.
For example <http://0.0.0.0:8001/main?sql=select%20\*%20from%20tasks>.


Some example views:

Equivalent to `tl head`:
```sql
select substr(ulid,22) as ulid,priority,body,due_utc,
  replace(tags,',',', ') as tags,notes,user
from tasks_view
where closed_utc is null
order by priority desc
limit 50
```

Make sure to bookmark the views for easy access.
