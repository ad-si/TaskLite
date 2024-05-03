.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: hackage-docs
hackage-docs:
	stack haddock tasklite-core --haddock-for-hackage
