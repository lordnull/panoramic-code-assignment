all: output/Server.js output/App.js output/index.html

output/Server.js: src/Server.purs
	spago bundle-app --main Server --to $@ --platform node

output/App.js: src/App.purs
	spago bundle-module --main App --to $@ --platform browser

output/index.html: src/index.html
	cp src/index.html $@
