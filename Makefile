# Makefile
build:
	ocamlbuild -use-ocamlfind -tag thread -pkg core -pkg cohttp -pkg cohttp.async -pkg yojson -pkg ppx_deriving_yojson main.d.byte
clean:
	ocamlbuild -clean
.PHONY: build clean
