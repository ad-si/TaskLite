.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format
format:
	fourmolu --mode inplace $$(fd -e hs)


.PHONY: start-app
start-app:
	cd tasklite-webapp \
	&& NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app start


.PHONY: lint
lint:
	hlint \
		--ignore-glob="archive" \
		--ignore="Redundant do" \
		--ignore="Use list literal" \
		--ignore="Use String" \
		--ignore="Redundant bracket" \
		--ignore="Use camelCase" \
		.


# Explicitly run tests for each package for better console output
.PHONY: test
test:
	stack test --fast tasklite-core
	stack test --fast tasklite


.PHONY: install  # Install `tasklite` with performance optimizations enabled.
# `force-dirty` is needed as stack doesn't recompile when only flags change.
install:
	stack build \
		--ghc-options '-O2' \
		--copy-bins \
		--force-dirty \
		tasklite


# Build the documentation
docs: book.toml docs-source
	stack haddock --haddock-for-hackage
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
	mdbook serve --port 4882


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


# Possible alternative: https://github.com/MrMarble/termsvg
tasklite/screenshots/help.svg:
	term-transcript exec 'tasklite help' > $@
	cp $@ docs-source/images/help.svg


tasklite/screenshots/help-short.svg:
	term-transcript exec 'tasklite help | head -n 30' > $@


# # TODO: Use an extra demo database to not mess up the local one
# # ATTENTION: Set correct commands before executing
# docs-source/images/usage.svg:
# 	term-transcript exec --io-timeout 1sec \
# 		'tl add Buy milk +groceries +demo' \
# 		'tl add Go running +demo' \
# 		'tl withtag demo' \
# 		'tl do TODO' \
# 		> $@


.PHONY: screenshots
screenshots: tasklite/screenshots/help.svg tasklite/screenshots/help-short.svg


.PHONY: release
release:
	@echo '1. Bump the version in `tasklite-core/package.yaml` and `tasklite/package.yaml`'
	@echo '2. Run `make test` to upgrade the cabal files'
	@echo '3. `git cliff` to generate changelog entries for the new version'
	@echo '4. Merge `_todo_changelog.md` into `docs-source/changelog.md`'
	@echo '5. `git add --interactive && git commit -m "Bump version"`'
	@echo '6. `git tag v?????`'
	@echo '7. `git push`'
	@echo '8. `make push-to-hackage`'
	@echo '9. Create a new GitHub release at https://github.com/ad-si/TaskLite/releases/new'
	@echo '10. Add artifacts from GitHub Action run'
	@echo '11. Announce the release on relevant channels'


.PHONY: push-to-hackage
push-to-hackage: docs
	stack upload tasklite-core
	stack upload --documentation tasklite-core

	stack upload tasklite
	# `tasklite` doesn't have documentation as it consists only of a `Main.hs`


.PHONY: clean
clean:
	rm -rf .stack-work
	rm -rf docs
	rm -rf tasklite-app/.stack-work
	rm -rf tasklite-core/.stack-work
	rm -rf tasklite/.stack-work
