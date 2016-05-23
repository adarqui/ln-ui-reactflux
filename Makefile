all:
	pulp browserify > static/dist/app.js

all-upload: all upload

upload:
	rsync -av -e ssh ./static/dist/app.js adarq:/root/projects/leuronet/ln-yesod/static/pure/ln.js
#	scp static/dist/app.js adarq:/root/projects/leuronet/ln-yesod/static/pure/ln.js

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
	bower install

wtf:
	rm -rf bower_components/purescript-daimyo
	bower install

deps:
	npm install -g pulp
