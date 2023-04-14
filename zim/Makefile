restart: serv
	sv restart ~/etc/service/skim-zim
serv: serv.c
	g++ -O3 -o serv serv.c -lzim
stop:
	sv stop ~/etc/service/skim-zim
b: a.c
	g++ -O3 -o b a.c -lzim
