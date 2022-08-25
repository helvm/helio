.PHONY: all bench build check check-whitespace clean configure exec fast golden haddock hlint hpack install main output repl report run sdist stan stylish test tix update

all: update fast bench

bench:
	rm -f helvm-common-benchmark.tix
	cabal new-bench --jobs -f ghcoptions

build:
	cabal new-build --jobs --enable-profiling -f ghcoptions

check:
	cabal check

check-whitespace:
	git check-whitespace

clean:
	cabal new-clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi
	if test -d .hie; then rm -r .hie; fi

configure:
	rm -f cabal.project.local*
	cabal configure --enable-benchmarks --enable-coverage --enable-tests -f ghcoptions

exec:
	make tix
	cabal new-exec --jobs helvm-common

fast: main report sdist install

golden:
	if test -d .output/golden; then rm -r .output/golden; fi

haddock:
	cabal new-haddock

hlint:
	#curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
	hlint . --report=hlint.html --timing
	mv hlint.html docs/reports

hpack:
	curl -sSL https://github.com/sol/hpack/raw/main/get-hpack.sh | bash

install:
	cabal install all --overwrite-policy=always

main:
	make stylish configure check build test

output:
	if test -d .output; then rm -r .output; fi

repl:
	cabal new-repl lib:helvm-common

report:
	make haddock stan hlint
	mkdir_and_cp() { mkdir -p $(dirname "$2") && cp -r "$1" "$2"}
	mkdir_and_cp dist-newstyle/build/*/*/*/doc/html/helvm-common docs/reports/doc
	mkdir_and_cp dist-newstyle/build/*/*/*/hpc/vanilla/html docs/reports/hpc

run:
	make tix
	cabal new-run --jobs helvm-common

sdist:
	cabal sdist

stan:
	export STAN_USE_DEFAULT_CONFIG=True
	stan -s --hide-solution report
	mv stan.html docs/reports

stylish:
	#curl -sL https://raw.github.com/haskell/stylish-haskell/master/scripts/latest.sh | sh -s "-r -v -i hs"
	stylish-haskell -r -v -i hs

test:
	cabal new-test --jobs --test-show-details=streaming -f ghcoptions

tix:
	rm -f helvm-common.tix

update:
	cabal update
