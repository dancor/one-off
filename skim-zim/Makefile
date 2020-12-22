all: serve restart-service


serve: src/serve.hs
	mkdir -p build; ghc --make -package http-types -package tagsoup \
		-package scotty -Wall src/serve -o serve -odir build -hidir build

restart-service:
	sv restart ~/etc/service/skim-zim
