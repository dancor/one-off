all: serve restart
serve: src/serve.hs
	mkdir -p build; ghc --make -package http-types -package tagsoup \
		-package scotty -Wall src/serve -o serve -odir build -hidir build
restart:
	sv restart ~/etc/service/skim-zim
stop:
	sv stop ~/etc/service/skim-zim
testserv:
	exec ~/p/o/skim-zim/serve ~/d/w/t/en.zim ~/d/w/t/es.zim ~/d/w/i/en.zim
