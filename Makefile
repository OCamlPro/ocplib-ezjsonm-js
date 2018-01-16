all: build

build: _obuild
	ocp-build build

install: _obuild
	ocp-build install

_obuild: Makefile
	ocp-build init

clean: _obuild
	ocp-build clean
	-find -name \*~ -delete

distclean: clean
	rm -rf _obuild

test: build
	nodejs _obuild/test/test.js
