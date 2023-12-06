.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f rummikaml.zip
	zip -r rummikaml.zip . -x@exclude.lst

doc:
	dune build @doc

opendoc: doc
				@bash opendoc.sh