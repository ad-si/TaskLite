.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: start-app
start-app:
	cd tasklite-airsequel-app \
	&& NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app start


# Explicitly run tests for each package for better console output
.PHONY: test
test:
	stack test --fast huzzy
	stack test --fast tasklite-core


.PHONY: install
install:
	stack build --fast --copy-bins


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
.PHONY: serve-docs
serve-docs:
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
	rm -rf .stack-work
	rm -rf huzzy/.stack-work
	rm -rf tasklite-api/.stack-work
	rm -rf tasklite-app/.stack-work
	rm -rf tasklite-core/.stack-work
	rm -rf tasklite-server/.stack-work
	rm -rf docs
