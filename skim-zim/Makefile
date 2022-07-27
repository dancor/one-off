all: serve restart stop


serve: src/serve.hs
	mkdir -p build; ghc --make -package http-types -package tagsoup \
		-package scotty -Wall src/serve -o serve -odir build -hidir build

restart:
	sv restart ~/etc/service/skim-zim

stop:
	sv stop ~/etc/service/skim-zim

testserv:
	exec ~/p/o/skim-zim/serve ~/data/wikt/en.zim ~/data/wikt/es.zim \
		~/data/wiki/en.zim
