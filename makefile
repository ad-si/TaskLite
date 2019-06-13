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


.PHONY: clean
clean:
	-rm -r docs
