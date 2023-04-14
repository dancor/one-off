restart: serv
	sv restart ~/etc/service/skim-zim
stop:
	sv stop ~/etc/service/skim-zim
serv: serv.c types.h err.h util.h
	g++ -O3 -o serv serv.c -lzim
b: a.c
	g++ -O3 -o b a.c -lzim
