OPTIMIZATION=-O0
build:
	cabal build all -j --ghc-options $(OPTIMIZATION)

.PHONY: test
test:
	cabal test -j --ghc-options $(OPTIMIZATION)

haddock:
	cabal haddock all

ghcid: clean
	ghcid \
		--test="main" \
		--command="ghci" \
		test/Test

ghcid-app: clean
	ghcid \
		--main="main" \
		--command="ghci" \
		app/Main

ghci:
	ghci app/Main

etags:
	hasktags  -e ./src

clean:
	rm -fR dist dist-*
	find . -name '*.hi' -type f -delete
	find . -name '*.o' -type f -delete
	find . -name '*.dyn_hi' -type f -delete
	find . -name '*.dyn_o' -type f -delete
	find . -name 'autogen*' -type f -delete

run:
	cabal run hsmin --ghc-options $(OPTIMIZATION) -- \

bootstrap:
	@echo "=== Bootstrap test: minify own source ==="
	cabal run hsmin -- src/HsMin.hs > /tmp/hsmin-bootstrap-HsMin.hs
	cabal run hsmin -- src/HsMin/Parse.hs > /tmp/hsmin-bootstrap-Parse.hs
	cabal run hsmin -- src/HsMin/Print.hs > /tmp/hsmin-bootstrap-Print.hs
	cabal run hsmin -- src/HsMin/Transform.hs > /tmp/hsmin-bootstrap-Transform.hs
	cabal run hsmin -- src/HsMin/Util.hs > /tmp/hsmin-bootstrap-Util.hs
	@echo "=== All source files minified successfully ==="
