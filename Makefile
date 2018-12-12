
build:
	dune build

clean:
	dune clean

install: build
	dune install

test:
	dune runtest

.PHONY:
	build clean test

