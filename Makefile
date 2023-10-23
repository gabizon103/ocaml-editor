.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

code:
	-dune build
	code .
	! dune build --watch

main:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f editor.zip 
	zip -r editor.zip . 

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
