default: test lint

test:
	stack test import-media

lint:
	hlint `find src test -type f -name '*.hs'`

build:
	stack build import-media

build-prof:
	stack build --profile --ghc-options="-rtsopts" import-media

clean:
	stack clean

hoogle:
	stack hoogle --server

.PHONY: \
	build \
	build-prof \
	clean \
	default \
	hoogle \
	lint \
	test \
