
build:
	dune build

clean:
	dune clean

install: build
	dune install


.PHONY:
	build clean

