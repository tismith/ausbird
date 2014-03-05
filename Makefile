all: build

.PHONY: init unsafe_init build doc test clean install run

init:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

unsafe_init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

install:
	cabal install

clean:
	rm -rf dist

run:
	cabal run

test: build
	cabal test

doc:
	cabal haddock --hyperlink-source
