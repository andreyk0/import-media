PATH:=.cabal-sandbox/bin:$(PATH)
export PATH

default: build

build:
	stack build

clean:
	stack clean

tags:
	hasktags --ctags --extendedctag  .

.PHONY: \
	clean \
	default \
	deps \
	init \
	tags \

