build:
	cabal install --only-dependencies --ghc-options="-j"
	cabal build
.PHONY: build
