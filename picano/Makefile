C=gcc
L=-lfluidsynth -lX11 -lXtst
O=-Wall -ffast-math -march=native -O3 $L

a: b.c err.h types.h
	$C -o $@ $< $O
