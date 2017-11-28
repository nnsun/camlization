play:
	ocamlbuild -pkg notty -pkg notty.unix -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean
