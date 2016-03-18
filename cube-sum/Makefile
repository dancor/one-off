all: v1 v2

v1: v1.c
	gcc -o v1 v1.c -lm -lgmp -O3

v2: v2.c
	gcc -march=native -o v2 v2.c -lm -lgmp -O3

cube-sum: cube-sum.c
	gcc -o cube-sum cube-sum.c -lm -O3

cube-sum-no-cache: cube-sum-no-cache.c
	gcc -o cube-sum-no-cache cube-sum-no-cache.c -lm -O3
