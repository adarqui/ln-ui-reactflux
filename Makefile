all:
	pulp browserify > static/dist/app.js

# https://www.npmjs.com/package/uglify-js
uglify:
	uglifyjs --compress --mangle -- static/dist/app.js 2>/dev/null > static/dist/app.min.js

all-upload: all upload

uglify-upload: all uglify upload-ugly

upload:
	rsync -av -e ssh ./static/dist/app.js adarq:/root/projects/leuronet/ln-ui/static/dist/app.js

upload-ugly:
	rsync -av -e ssh ./static/dist/app.min.js adarq:/root/projects/leuronet/ln-ui/static/dist/app.js

prod:
	psc-bundle output/**/*.js -m Main --main Main > ./static/dist/app.js

prod-uglify: prod uglify

upload-prod: prod-uglify upload-ugly

id:
	pscid --censor-codes=ImplicitImport,UnusedExplicitImport,HidingImport,WildcardInferredType,ImplicitQualifiedImport,DeprecatedOperatorDecl

build:
	pulp -w build

build-psa:
	pulp -w build --stash --censor-lib --censor-codes=ImplicitImport,UnusedExplicitImport,HidingImport,WildcardInferredType,ImplicitQualifiedImport,DeprecatedOperatorDecl

build-psa-no-opts:
	pulp -w build --stash --censor-lib --censor-codes=ImplicitImport,UnusedExplicitImport,HidingImport,WildcardInferredType,ImplicitQualifiedImport,DeprecatedOperatorDecl --no-opts

tests:
	pulp -w test

bower:
	bower install

reset:
	rm -rf bower_components/purescript-ln
	rm -rf bower_components/purescript-bbcode-parser
	bower install

wtf:
	rm -rf bower_components/purescript-daimyo
	bower install

deps:
	npm install -g pulp
