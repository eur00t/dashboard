#!/usr/bin/env bash

JS_HASH_OUTPUT=$(shasum public/bundle.js)
CSS_HASH_OUTPUT=$(shasum public/bundle.css)
JS_HASH=${JS_HASH_OUTPUT:0:40}
CSS_HASH=${CSS_HASH_OUTPUT:0:40}

mv public/bundle.js public/cached/bundle-$JS_HASH.js
mv public/bundle.css public/cached/bundle-$CSS_HASH.css