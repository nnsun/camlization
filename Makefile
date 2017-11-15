play:
	ocamlbuild -use-ocamlfind main.byte && ./_build/main.byte

clean:
	ocamlbuild -clean
