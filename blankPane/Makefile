C=g++ -std=gnu++20
L=-lxcb -lxcb-shm
O=-Wall -ffast-math -march=native -O3 $L
#../../templates/c/xwin.h
a: b.c ../../template/c/xwin.h ../../template/c/color.h
	$C -o $@ $< $O
