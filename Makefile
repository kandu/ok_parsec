PROJECT= ok_parsec

.PHONY: build, install clean

build:
	dune build

install: build
	dune install

uninstall: build
	dune uninstall

clean:
	dune clean

