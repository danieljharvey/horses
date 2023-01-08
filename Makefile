HS_FILES = $(shell git ls-files '*.hs')

.PHONY: ghcid
ghcid:
	ghcid -c "cabal repl mimsa" -l=hlint

.PHONY: ghcid-core
ghcid-core:
	ghcid -c "cabal repl core" -l=hlint

.PHONY: ghcid-core-test
ghcid-core-test:
	ghcid -c "cabal repl core:test:core-test" -l=hlint

.PHONY: ghcid-test
ghcid-test:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint

.PHONY: ghcid-repl
ghcid-repl:
	ghcid -c "cabal repl repl:exe:mimsa-repl" -l=hlint

.PHONY: ghcid-server
ghcid-server:
	ghcid -c "cabal repl server:exe:mimsa-server" -l=hlint

.PHONY: ghcid-smol
ghcid-smol:
	ghcid -c "cabal repl smol-core"

.PHONY: ghcid-smol-test
ghcid-smol-test:
	ghcid -c "cabal repl smol-core:test:smol-core-tests" --test "main"

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all

.PHONY: install
install:
	cabal install repl:exe:mimsa-repl --overwrite-policy=always
	cabal install server:exe:mimsa-server --overwrite-policy=always

.PHONY: run-server
run-server:
	cabal run server:exe:mimsa-server

.PHONY: test
test:
	cabal run mimsa:test:mimsa-test

.PHONY: test-smol
test-smol:
	cabal run smol-core:test:smol-core-tests


.PHONY: test-core
test-core:
	cabal run core:test:core-test

.PHONY: test-watch
test-watch:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint --test="main"

.PHONY: freeze
freeze:
	cabal freeze --enable-tests --enable-benchmarks

.PHONY: bench
bench:
	cabal bench mimsa

.PHONY: format
format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"

.PHONY: generate-swagger
generate-swagger: install
	$(shell cabal list-bin server:exe:mimsa-server) generate-swagger
