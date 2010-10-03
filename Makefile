all:	build

config:
	cabal configure

build:
	cabal build

install:	config build
	cabal install

test:
	./tests/runtests

clean:
	rm -rf dist
