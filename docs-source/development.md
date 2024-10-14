# Development

---
<!-- toc -->
---

## Technologies

- Programming language: [Haskell]
- Dependency management: [Stack]
- Backend: [SQLite]
- Database access: [sqlite-simple]
- Command line parsing: [Optparse Applicative]
- Formatting: [Prettyprinter]
- Prelude: [Protolude]
- IDs: [ULID]
- Desktop App: [Declarative GTK]

[Declarative GTK]: https://github.com/owickstrom/gi-gtk-declarative
[Haskell]: https://haskell.org
[Optparse Applicative]: https://github.com/pcapriotti/optparse-applicative
[Prettyprinter]: https://github.com/quchen/prettyprinter
[Protolude]: https://github.com/sdiehl/protolude
[sqlite-simple]: https://github.com/nurpax/sqlite-simple
[SQLite]: https://sqlite.org
[Stack]: https://docs.haskellstack.org
[ULID]: https://github.com/ulid/spec


## Development Environment

The recommended way to develop Haskell
is with [VS Code] and the [Haskell Language Server].

[VS Code]: https://code.visualstudio.com
[Haskell Language Server]:
  https://marketplace.visualstudio.com/items?itemName=haskell.haskell


## Getting Started

Check out the [makefile] for all development tasks.

[makefile]: https://github.com/ad-si/TaskLite/blob/master/makefile

The most important command is `make test` to run the tests after any changes.
They should always pass before committing.

To try out local changes via the CLI you can use the following command:

```sh
stack run -- add "Buy milk"
```


## Deployment

### Google Cloud

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
