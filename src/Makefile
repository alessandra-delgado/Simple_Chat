all:
	ocamlfind ocamlopt -linkpkg -package eio,eio_main -thread -o server server.ml
	ocamlfind ocamlopt -linkpkg -package eio,eio_main -thread -o client client.ml
	rm -f *.cmi *cmx *.o

clean:
	rm -f server client