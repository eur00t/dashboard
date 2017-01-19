# Makefile
build:
	ocamlbuild -Is shared0 -use-ocamlfind -tag thread \
	-pkg core \
	-pkg cohttp \
	-pkg cohttp.async \
	-pkg yojson \
	-pkg ppx_deriving_yojson \
	-pkg ppx_here \
	-pkg ppx_sexp_conv \
	-pkg websocket \
	-pkg websocket.async \
	main.d.byte
clean:
	ocamlbuild -clean

buildjs:
	ocamlbuild -Is shared0 -use-ocamlfind -tag thread \
	-pkg js_of_ocaml \
	-pkg ppx_deriving_yojson \
	-pkg js_of_ocaml.ppx \
	-cflag -g \
	client.d.byte
	js_of_ocaml --pretty --no-inline --debug-info --source-map +nat.js client.d.byte

buildjs_prod:
	ocamlbuild -Is shared0 -use-ocamlfind -tag thread \
	-pkg js_of_ocaml \
	-pkg ppx_deriving_yojson \
	-pkg js_of_ocaml.ppx \
	-cflag -g \
	client.d.byte
	js_of_ocaml --opt 3 +nat.js client.d.byte

.PHONY: build clean
