build: 
	nix-build -A mimsa.components.exes.mimsa

run:
	make build
	./result/bin/mimsa

build-test:
	nix-build -A mimsa.components.tests.mimsa-test

unit-test:
	make build-test
	./result/bin/mimsa-test
