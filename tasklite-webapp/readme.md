# TaskLite Web App

The TaskLite web app is a simple [Elm Land](https://elm.land/) app
backed by an [AirGQL](https://github.com/Airsequel/AirGQL) GraphQL server.

![Screenshot of web app](images/2024-05-03t1220_screenshot.png)


## Development

1. Start the TaskLite server with `tasklite server`.
1. Build GraphQL connector code with `make generate-api`.
1. Start the development server with `make start`.

You can now access the app at <http://localhost:7459>.
The TaskLite server only accepts cross-origin requests
from this development server.
