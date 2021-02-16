all: build

build:
	dune build --profile release

install:
	dune install

clean:
	dune clean
	-find -name \*~ -delete

test: build
	nodejs _build/test/test.bc.js
