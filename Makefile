.PHONY : feedback all clean distclean lint

all : feedback

.cabal-sandbox/ cabal.sandbox.config : feedback.cabal
	cabal sandbox init
	cabal install -j --only-dependencies

dist : .cabal-sandbox/ cabal.sandbox.config
	cabal configure

feedback : dist .cabal-sandbox/ cabal.sandbox.config
	cabal build -j
	strip -o feedback dist/build/feedback/feedback

clean :
	rm -rf dist/

distclean : clean
	rm -rf feedback .cabal-sandbox/ cabal.sandbox.config
	which git >/dev/null && git clean -xnd

lint :
	hlint src
