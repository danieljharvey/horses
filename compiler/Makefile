.PHONY: all test 

build-nix: 
	nix-build -A hsPkgs.mimsa.components.exes.mimsa --quiet | cachix push mimsa

run-nix:
	make build-nix
	./result/bin/mimsa

build-test-nix:
	nix-build pkgs.nix -A hsPkgs.mimsa.components.tests.mimsa-test --quiet | cachix push mimsa

unit-test-nix:
	make build-test-nix
	./result/bin/mimsa-test

ghcid:
	ghcid -c "stack repl --test --bench" -l=hlint

build:
	stack build --test --no-run-tests --bench --no-run-benchmarks 

test-watch:
	stack test --file-watch --bench --no-run-benchmarks mimsa:mimsa-test

errors-watch:
	stack test --file-watch --bench --no-run-benchmarks mimsa:errors-test

test:
	stack test --bench --no-run-benchmarks

bench:
	stack bench --test --no-run-tests 

profile-test:
	stack build --test --profile && stack exec --profile -- test +RTS

hoogle:
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8090

haddock:
	stack build --fast --haddock-deps
	stack haddock --open
