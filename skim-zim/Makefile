all: serve restart-service


serve: src/serve.hs
	mkdir -p build; ghc --make -Wall src/serve -o serve -odir build -hidir build

restart-service:
	sv restart ~/etc/service/skim-zim
