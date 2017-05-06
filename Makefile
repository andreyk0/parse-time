build:
	stack build $(STACK_OPTS) parse-time

test:
	stack test parse-time

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build clean tags sources test

