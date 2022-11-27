.PHONY: all ghcid ghcid-repl ghcid-server ghcid-test test install

ghcid:
	ghcid -c "cabal repl mimsa" -l=hlint

ghcid-test:
	ghcid -c "cabal repl mimsa:tests:mimsa-test" -l=hlint

ghcid-repl:
	ghcid -c "cabal repl mimsa:exe:mimsa" -l=hlint

ghcid-server:
	ghcid -c "cabal repl mimsa:exe:mimsa-server" -l=hlint

build:
	cabal update
	cabal build all

install:
	cabal update
	cabal install mimsa:exe:mimsa --overwrite-policy=always

run-server:
	cabal run mimsa:exe:mimsa-server

test-watch:
	ghcid -c "cabal repl mimsa:tests" -l=hlint --test="main"

test:
	cabal update
	cabal run mimsa:test:mimsa-test

bench:
	cabal update
	cabal bench
