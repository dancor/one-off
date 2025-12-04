.PHONY: run
C=gcc -O3 -Wall -Werror -ffast-math -fmax-errors=1 -fomit-frame-pointer \
	-march=native -o
ra: a
	./$^ > out
a: b.c
	$C $@ $^
