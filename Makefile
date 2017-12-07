build-play:
	make build && make play

build:
	ocamlbuild -pkg notty -pkg notty.unix -use-ocamlfind main.byte

play:
	./main.byte

debug:
	ocamlfind ocamlopt -g -o main -linkpkg -package notty,notty.unix \
	primitives.ml world.mli world.ml tech.mli tech.ml entity.mli entity.ml \
	player.mli player.ml notty_helper.ml state.mli state.ml gui.mli gui.ml \
	menu.mli menu.ml main.ml

clean:
	ocamlbuild -clean 

test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte
