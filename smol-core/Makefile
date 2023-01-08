HS_FILES=$(shell git ls-files '*.hs')

.PHONY: ghcid
ghcid:
	ghcid -c "cabal repl"

.PHONY: ghcid-test
ghcid-test:
	ghcid -c "cabal repl types-zoo-tests" --test "main"

.PHONY: ghcid-app
ghcid-app:
	ghcid -c "cabal repl types-zoo:exe:types-zoo"

.PHONY: run
run:
	cabal run types-zoo

.PHONY: format
format:
	ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"
