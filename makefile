.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: start-app
start-app:
	cd tasklite-airsequel-app \
	&& NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app start


.PHONY: test
test:
	stack test


# Build the documentation
docs: book.toml docs-source
	mdbook build


# Build the documentation with Docker
.PHONY: docker-docs
docker-docs: book.toml docs-source
	docker run \
		--rm \
		--volume "$$PWD":/data \
		hrektts/mdbook \
		mdbook build


# Continuously rebuild and serve at localhost:3000
.PHONY: serve
serve:
	mdbook serve


# Continuously rebuild and serve at localhost:3000 with Docker
.PHONY: docker-serve
docker-serve: book.toml docs-source
	docker run \
		--rm \
		--volume "$$PWD":/data \
		--publish 3000:3000 \
		--publish 3001:3001 \
		hrektts/mdbook \
		mdbook serve --hostname 0.0.0.0


.PHONY: deploy
deploy: fly.toml
	fly deploy


.PHONY: docker-build
docker-build:
	# Make sure to have at least 8 GB RAM allocated for the Docker Engine
	# as the compilation is quite resource intensive.
	# Otherwise you'll get an 137 or -9 error.

	docker build \
		--tag adius/tasklite:$$(git describe --tags --dirty) \
		--tag adius/tasklite:latest \
		.


.PHONY: clean
clean:
	-rm -r .stack-work
	-rm -r huzzy/.stack-work
	-rm -r tasklite-api/.stack-work
	-rm -r tasklite-app/.stack-work
	-rm -r tasklite-core/.stack-work
	-rm -r tasklite-server/.stack-work
	-rm -r docs
