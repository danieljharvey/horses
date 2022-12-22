.PHONY: all ghcid ghcid-repl ghcid-server ghcid-test test install

HS_FILES = $(shell git ls-files '*.hs')


ghcid:
	ghcid -c "cabal repl mimsa" -l=hlint

ghcid-test:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint

ghcid-repl:
	ghcid -c "cabal repl mimsa:exe:mimsa" -l=hlint

ghcid-server:
	ghcid -c "cabal repl server:exe:mimsa-server" -l=hlint

build:
	cabal update
	cabal build all

install:
	cabal update
	cabal install mimsa:exe:mimsa --overwrite-policy=always

run-server:
	cabal run server:exe:mimsa-server

test-watch:
	ghcid -c "cabal repl mimsa:test:mimsa-test" -l=hlint --test="main"

freeze:
	cabal freeze --enable-tests --enable-benchmarks

test:
	cabal update
	cabal run mimsa:test:mimsa-test

bench:
	cabal update
	cabal bench

format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"
