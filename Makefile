build:
	cabal install -j --only-dependencies --ghc-options="-j"
	cabal build
.PHONY: build
