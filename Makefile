.PHONY: buildjs_prod buildcss embed_hash html

build:
	ocamlbuild -Is shared0,processors -use-ocamlfind -tag thread \
	-pkg core \
	-pkg cohttp \
	-pkg cohttp.async \
	-pkg yojson \
	-pkg ppx_deriving_yojson \
	-pkg ppx_here \
	-pkg ppx_sexp_conv \
	-pkg websocket \
	-pkg conduit.async \
	-pkg websocket.async \
	main.native
clean:
	ocamlbuild -clean

buildjs:
	ocamlbuild -Is shared0,processors -use-ocamlfind -tag thread \
	-pkg js_of_ocaml \
	-pkg ppx_deriving_yojson \
	-pkg js_of_ocaml.ppx \
	-pkg reactjs \
	-pkg commonjs \
	-pkg commonjs_ppx \
	-cflag -g \
	client_main.d.byte
	js_of_ocaml --pretty --no-inline --debug-info --source-map +nat.js client_main.d.byte

buildjs_prod:
	ocamlbuild -Is shared0,processors -use-ocamlfind -tag thread \
	-pkg js_of_ocaml \
	-pkg ppx_deriving_yojson \
	-pkg js_of_ocaml.ppx \
	-pkg reactjs \
	-pkg commonjs \
	-pkg commonjs_ppx \
	-cflag -g \
	client_main.d.byte
	js_of_ocaml --opt 3 +nat.js client_main.d.byte
	cat node_modules/react/dist/react.min.js \
		node_modules/react-dom/dist/react-dom.min.js \
		node_modules/highcharts/highcharts.js \
		client_main.d.js > public/bundle.js

buildcss:
	sassc --style compressed styles/scss/main.scss styles/css/main.css
	cat node_modules/bootstrap/dist/css/bootstrap-reboot.min.css \
		styles/css/main.css > public/bundle.css

html:
	scripts/build_html.bash

embed_hash:
	scripts/embed_hash.bash

build_client: clean_client buildjs_prod buildcss html embed_hash

clean_client:
	rm -f public/cached/*
	rm -f public/index.html

.PHONY: build clean
