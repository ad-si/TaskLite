- Always run `make test` after any changes to ensure everything is working correctly.
- `tasklite server` (port 7458) serves the static build from
  `tasklite-webapp/build`.
  After changing the web app, run `make build` in `tasklite-webapp`
  and verify the result visually (e.g. headless Chrome screenshot)
  before reporting it as fixed.
