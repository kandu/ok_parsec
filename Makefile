PROJECT= ok_parsec

LIBS= ok_parsec.cma ok_parsec.cmxa

jobs= 2

libs: $(LIBS)

ok_parsec.cma: common.ml fa.ml parsec.ml re.ml
	ocamlbuild -tags annot,debug -j $(jobs) $@

ok_parsec.cmxa: common.ml fa.ml parsec.ml re.ml
	ocamlbuild -tags annot,debug -j $(jobs) $@

.PHONY: install clean

install: $(LIBS)
	cd _build; ocamlfind install $(PROJECT) ../META ok_parsec.cmi ok_parsec.a *.mli $(LIBS)

uninstall:
	ocamlfind remove $(PROJECT)

clean:
	ocamlbuild -clean

test: test.byte
	./test.byte
test.byte: test.ml
	ocamlbuild -tags annot,debug -j $(jobs) $@

