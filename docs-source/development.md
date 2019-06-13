# Development

Main technologies:

- Programming language: [Haskell]
- Dependency management: [Stack]
- Backend: [SQLite]
- Database access: [Beam]
- Command line parsing: [Optparse Applicative]
- Formatting: [Prettyprinter]
- Prelude: [Protolude]
- IDs: [ULID]
- Desktop App: [Declarative GTK]

[Beam]: https://tathougies.github.io/beam/
[Declarative GTK]: https://github.com/owickstrom/gi-gtk-declarative
[Haskell]: https://haskell.org
[Optparse Applicative]: https://github.com/pcapriotti/optparse-applicative
[Prettyprinter]: https://github.com/quchen/prettyprinter
[Protolude]: https://github.com/sdiehl/protolude
[SQLite]: https://sqlite.org
[Stack]: https://docs.haskellstack.org
[ULID]: https://github.com/ulid/spec


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
  --profile ~/dotfiles/terminal/adius.itermcolors \
  < screenshots/recording.json \
  > screenshots/recording.svg
```

[svg-term]: https://github.com/marionebl/svg-term-cli


## Documentation

Build the documentation with following command:

```sh
docker run \
  --rm \
  --volume "$PWD":/data \
  hrektts/mdbook \
  mdbook build
```


### Ghcid

Ghcid with color output for GHC 8.4 (probably obsolete in 8.6):

```sh
ghcid \
  --command="stack ghci --ghci-options=-fdiagnostics-color=always"
```


### Hlint

```sh
hlint \
  --ignore="Redundant do" \
  --ignore="Use list literal" \
  --ignore="Use String" \
  --ignore="Redundant bracket" \
  --ignore="Use camelCase" \
  .
```


## Webapp

### Build Images

Build base image for webapp runtime image:

```sh
docker build \
  --file tasklite-core/dockerfiles/haskell-datasette \
  --tag haskell-datasette \
  dockerfiles
```

Build runtime image:

```sh
stack image container
docker tag adius/tasklite-tasklite:latest adius/tasklite:latest
```


### Deployment

On Google Cloud:

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
for the load balancer at
<https://console.cloud.google.com/compute/healthChecks>.
