#!/usr/bin/env bash

JS_HASH_OUTPUT=$(shasum public/bundle.js)
CSS_HASH_OUTPUT=$(shasum public/bundle.css)
JS_HASH=${JS_HASH_OUTPUT:0:40}
CSS_HASH=${CSS_HASH_OUTPUT:0:40}

cat index_templ.html | \
    sed -e "s/{{js_hash}}/$JS_HASH/g" \
        -e "s/{{css_hash}}/$CSS_HASH/g" > public/index.html