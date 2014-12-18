PROJECT= ok_parsec

lib= ok_parsec.cma ok_parsec.cmxa

jobs= 2

ok_parsec.cma: common.ml fa.ml parsec.ml re.ml
	ocamlbuild -tags annot,debug -j $(jobs) -use-ocamlfind $@

ok_parsec.cmxa: common.ml fa.ml parsec.ml re.ml
	ocamlbuild -tags annot,debug -j $(jobs) -use-ocamlfind $@

.PHONY: install clean

install: $(lib)
	cd _build; ocamlfind install $(PROJECT) ../META ok_parsec.cmi ok_parsec.a $(lib)

uninstall:
	ocamlfind remove $(PROJECT)

clean:
	ocamlbuild -clean

test: test.byte
	./test.byte
test.byte: test.ml
	ocamlbuild -tags annot,debug j -use-ocamlfind $@

