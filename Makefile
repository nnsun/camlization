buildplay:
	make build && make play

build:
	ocamlbuild -pkg notty -pkg notty.unix -use-ocamlfind main.byte

play:
	./main.byte

clean:
	ocamlbuild -clean
