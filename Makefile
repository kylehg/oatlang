main: *.ml
	ocamlbuild -lib unix -lib str main.native main.byte

test: main
	./main.native -I lib -runtime ./runtime.c --test

build: main
	ocamlrun -b main.byte --show-il --show-ast $F

clean:
	rm c_bin/* c_obj/*
