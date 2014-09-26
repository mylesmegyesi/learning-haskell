.PHONY: deps
deps:
	cabal install --only-dependencies

.PHONY: test
test:
	cabal build spec
	hlint src test
	./dist/build/spec/spec ${ARGS}

.PHONY: clean
clean:
	rm -rf dist
