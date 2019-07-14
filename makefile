# Build the documentation
docs:
	docker run \
		--rm \
		--volume "$$PWD":/data \
		hrektts/mdbook \
		mdbook build


.PHONY: deploy
deploy: docs
	echo 'tasklite.ad-si.com' > docs/CNAME
	surge docs


.PHONY: docker-build
docker-build:
	# Make sure to have at least 2 GB RAM allocated for the Docker Engine
	# as the compilation is quite resource intensive.
	# Otherwise you'll get an 137 error.

	docker build \
		--tag adius/tasklite \
		.


.PHONY: clean
clean:
	-rm -r docs
