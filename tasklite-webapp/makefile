.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


.PHONY: format
format:
	elmfmt -i src


.PHONY: start
start: node_modules
	NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app start --no-debug --no-browser


.PHONY: build
build: node_modules
	NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app build --no-debug --no-browser


.PHONY: generate-api
generate-api: node_modules
	npx elm-graphql --skip-elm-format http://localhost:7458/graphql
	$(MAKE) format


.PHONY: clean
clean:
	-rm -rf elm-stuff
	-rm -rf node_modules
