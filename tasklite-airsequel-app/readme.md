# TaskLite Airsequel App

App for a [TaskLite](tasklite.org) database
hosted on [Airsequel](airsequel.com)
written in [Elm](elm-lang.org).


## Development

1. Install dependencies with `npm install`.
1. Build GraphQL connector code:
    ```sh
    npx elm-graphql --skip-elm-format \
      http://localhost:4185/dbs/tasklite/graphql
    ```
1. Start the development server by running:
    ```sh
    npx elm reactor
    ```

You can now access the app at <localhost:8000/src/>.
