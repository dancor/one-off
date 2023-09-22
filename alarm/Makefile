C=g++ -std=gnu++20
# 2020 standard has source_location. 2017 doesn't.
L=
#L=-lSDL2 -lGL -lGLEW
O=-Wall -ffast-math -march=native -O3 $L
a: b.c ../../templates/cpp/err.h
	$C -o $@ $< $O
