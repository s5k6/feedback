targets = feedback grades

.PHONY : $(targets) all clean distclean lint

all : $(targets)

.cabal-sandbox/ cabal.sandbox.config : feedback.cabal
	cabal sandbox init
	cabal install -j --only-dependencies

dist : .cabal-sandbox/ cabal.sandbox.config
	cabal configure

feedback grades : dist .cabal-sandbox/ cabal.sandbox.config
	cabal build -j $@
	strip -o $@ dist/build/$@/$@

clean :
	rm -rf dist/

distclean : clean
	rm -rf $(targets)
	rm -rf .cabal-sandbox/ cabal.sandbox.config
	which git >/dev/null && git clean -xnd

lint :
	hlint src
