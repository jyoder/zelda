ifneq ($(wildcard IHP/.*),)
IHP = IHP/lib/IHP
else
IHP = $(shell dirname $$(which RunDevServer))/../lib/IHP
endif

CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css
CSS_FILES += static/app.css

JS_FILES += ${IHP}/static/vendor/jquery-3.6.0.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/morphdom-umd.min.js
JS_FILES += ${IHP}/static/vendor/turbolinks.js
JS_FILES += ${IHP}/static/vendor/turbolinksInstantClick.js
JS_FILES += ${IHP}/static/vendor/turbolinksMorphdom.js
JS_FILES += static/halogen/index.js

include ${IHP}/Makefile.dist

static/halogen/index.js:
	HOME=/tmp npm ci
	HOME=/tmp spago build --purs-args "--output static/halogen/output" --source-maps --then "npm run bundle"
	esbuild static/halogen/main.js --bundle --outfile=static/halogen/index.js --minify --sourcemap
