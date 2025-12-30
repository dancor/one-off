.phony: rA
C=gcc -O3 -Wall -Werror -ffast-math -fmax-errors=1 -fomit-frame-pointer \
	-march=native -o
rB: b
	/usr/bin/time ./$^
b: bitLock.c
	$C $@ $^ -pthread -lxxhash -lz
