node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


.PHONY: start
start: node_modules
	NODE_OPTIONS=--openssl-legacy-provider \
	npx elm-app start --no-debug --no-browser


.PHONY: clean
clean:
	-rm -rf elm-stuff
	-rm -rf node_modules
