all:	build

config:
	cabal configure

build:
	cabal build

install:	config build
	cabal install

clean:
	rm -rf dist
