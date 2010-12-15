all:	build

config:
	cabal configure

build:
	cabal build

install:
	cabal install

test:
	./tests/runtests

clean:
	rm -rf dist
