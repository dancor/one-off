C=gcc
L=-lasound -lm
O=-ffast-math -march=native -O3 -Wall -Werror -Wfatal-errors $L
sound: main.c
	$C -o $@ $< $O
