PATH:=.cabal-sandbox/bin:$(PATH)
export PATH

default: build

%:
	cabal $@

init:
	cabal sandbox init

deps:
	cabal install --dependencies-only

clean:
	cabal clean

tags:
	hasktags --ctags --extendedctag  . ~/.cabal/packages/

.PHONY: \
	clean \
	default \
	deps \
	init \
	tags \

