build-play:
	make build && make play

build:
	dune build main.exe --profile release

play:
	_build/default/main.exe

debug:
	ocamlfind ocamlopt -g -o main -linkpkg -package notty,notty.unix \
	primitives.ml world.mli world.ml tech.mli tech.ml entity.mli entity.ml \
	player.mli player.ml notty_helper.ml state.mli state.ml gui.mli gui.ml \
	menu.mli menu.ml main.ml

clean:
	dune clean

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte
