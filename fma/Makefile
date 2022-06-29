O=-ffast-math -march=native -O3

%.s: %.c
	gcc -o $@ -S $< $O

%: %.c
	gcc -o $@ $< $O
