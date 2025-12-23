.phony: rDay
C=gcc -O3 -Wall -Werror -ffast-math -fmax-errors=1 -fomit-frame-pointer \
	-march=native -o
rDay: day
	/usr/bin/time ./$^ gzDay/* > tmpOut
day: day.c
	$C $@ $^ -pthread -lxxhash -lz
