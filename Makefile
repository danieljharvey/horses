HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')

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

.PHONY: ghcid-backends
ghcid-backends:
	ghcid -c "cabal repl backends:lib:backends" -l=hlint

.PHONY: ghcid-backends-test
ghcid-backends-test:
	ghcid -c "cabal repl backends:test:backends-tests" --test "main"

# EXCITING NEW WORLD

.PHONY: ghcid-smol
ghcid-smol:
	ghcid -c "cabal repl smol-core"

.PHONY: ghcid-smol-test
ghcid-smol-test:
	ghcid -c "cabal repl smol-core:test:smol-core-tests" --test "main"

.PHONY: ghcid-smol-backend-test
ghcid-smol-backend-test:
	ghcid -c "cabal repl smol-backend:test:smol-backend-tests" --test "main"

.PHONY: ghcid-smol-repl
ghcid-smol-repl:
	ghcid -c "cabal repl smol-repl"

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4

.PHONY: install
install:
	cabal install repl:exe:mimsa-repl --overwrite-policy=always

.PHONY: smol-repl
smol-repl:
	cabal run smol-repl:exe:smol-repl

.PHONY: run-server
run-server:
	cabal run server:exe:mimsa-server

.PHONY: docker-server
docker-server:
	docker build docker/Dockerfile.server

.PHONY: test
test:
	cabal run mimsa:test:mimsa-test

.PHONY: test-smol
test-smol:
	cabal run smol-core:test:smol-core-tests

.PHONY: test-smol-backend
test-smol-backend:
	cabal run smol-backend:test:smol-backend-tests

.PHONY: test-core
test-core:
	cabal run core:test:core-test

.PHONY: test-llvm-calc
test-llvm-calc:
	cabal run llvm-calc:tests

.PHONY: test-llvm-calc2
test-llvm-calc2:
	cabal run llvm-calc2:tests

.PHONY: test-llvm-calc3
test-llvm-calc3:
	cabal run llvm-calc3:tests

.PHONY: test-llvm-calc4
test-llvm-calc4:
	cabal run llvm-calc4:tests

.PHONY: build-smol-repl
build-smol-repl:
	cabal build smol-repl

.PHONY: test-backends
test-backends:
	cabal run backends:test:backends-tests

.PHONY: test-watch
test-watch:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint --test="main"

.PHONY: freeze
freeze:
	cabal freeze --enable-tests --enable-benchmarks

.PHONY: bench
bench:
	cabal bench benchmarks:benchmarks

.PHONY: format
format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"

.PHONY: hlint
hlint:
	@hlint $(HS_FILES)

.PHONY: format-cabal
format-cabal:
	@cabal-fmt -i $(CABAL_FILES)
