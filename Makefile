build:
	cabal install -j --only-dependencies --ghc-options="-j"
	cabal build
	strip ./dist/build/par/par
.PHONY: build
install:
	cp ./dist/build/par/par /usr/local/bin/
	chmod +x /usr/local/bin/par
.PHONY: install
