.PHONY: all ghcid ghcid-repl ghcid-server test install

ghcid:
	ghcid -c "cabal repl mimsa" -l=hlint

ghcid-repl:
	ghcid -c "cabal repl mimsa:exe:mimsa" -l=hlint

ghcid-server:
	ghcid -c "cabal repl mimsa:exe:mimsa-server" -l=hlint

build:
	cabal build all

install:
	cabal install mimsa:exe:mimsa --overwrite-policy=always

test-watch:
	ghcid -c "cabal repl mimsa:tests" -l=hlint --test="main"

test:
	cabal run mimsa:test:mimsa-test

bench:
	cabal bench
