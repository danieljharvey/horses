HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CHANGED_HS_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/trunk` | grep '.*\(\.hs\|hs-boot\)$$' | grep -E -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')

.PHONY: ghcid-smol
ghcid-smol:
	ghcid -c "cabal repl smol-core"

.PHONY: ghcid-smol-test
ghcid-smol-test:
	ghcid -c "cabal repl smol-core:test:smol-core-tests" --test "main"

.PHONY: ghcid-smol-interpreter-test
ghcid-smol-interpreter-test:
	ghcid -c "cabal repl smol-interpreter:test:smol-interpreter-tests" --test "main"

.PHONY: ghcid-smol-modules-test
ghcid-smol-modules-test:
	ghcid -c "cabal repl smol-modules:test:smol-modules-tests" --test "main"

.PHONY: ghcid-smol-backend-test
ghcid-smol-backend-test:
	ghcid -c "cabal repl smol-backend:test:smol-backend-tests" --test "main"

.PHONY: ghcid-smol-wasm
ghcid-smol-wasm:
	ghcid -c "cabal repl smol-wasm"

.PHONY: ghcid-smol-wasm-test
ghcid-smol-wasm-test:
	ghcid -c "cabal repl smol-wasm:test:smol-wasm-tests" --test "main"

.PHONY: ghcid-smol-repl
ghcid-smol-repl:
	ghcid -c "cabal repl smol-repl"

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4

.PHONY: smol-repl
smol-repl:
	cabal run smol-repl:exe:smol-repl -- repl

CHECK_FILE = "file.smol"
.PHONY: smol-check
smol-check:
	watchexec -w $(CHECK_FILE) cabal run smol-repl:exe:smol-repl -- check $(CHECK_FILE)

.PHONY: docker-server
docker-server:
	docker build docker/Dockerfile.server

# used in CI

.PHONY: test-smol
test-smol:
	cabal run smol-core:test:smol-core-tests

.PHONY: test-smol-backend
test-smol-backend:
	cabal run smol-backend:test:smol-backend-tests

.PHONY: test-smol-interpreter
test-smol-interpreter:
	cabal run smol-interpreter:test:smol-interpreter-tests

.PHONY: test-smol-modules
test-smol-modules:
	cabal run smol-modules:test:smol-modules-tests

.PHONY: test-smol-wasm
test-smol-wasm:
	cabal run smol-wasm:test:smol-wasm-tests

# yep

.PHONY: build-smol-repl
build-smol-repl:
	cabal build smol-repl

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

.PHONY: hlint-apply
hlint-apply:
	@echo $(HS_FILES) | xargs -n1 hlint --refactor --refactor-options='--inplace'

.PHONY: hlint-changed
hlint-changed:
	@hlint $(CHANGED_HS_FILES)

.PHONY: hlint-apply-changed
hlint-apply-changed:
	@echo $(CHANGED_HS_FILES) | xargs -n1 hlint --refactor --refactor-options='--inplace'

.PHONY: format-cabal
format-cabal:
	@cabal-fmt -i $(CABAL_FILES)
