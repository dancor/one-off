.phony: rF
C=gcc -O3 -Wall -Werror -ffast-math -fmax-errors=1 -fomit-frame-pointer \
	-march=native
tt: tt.c printXev.h
	$C -I/usr/include/cairo -I/usr/include/drm -I/usr/include/glib-2.0 \
		-I/usr/lib/glib-2.0/include -I/usr/include/harfbuzz \
		-I/usr/include/pango-1.0 "-L${HOME}/.local/lib" \
		"-Wl,-rpath,${HOME}/.local/lib" -lX11 -lXext -lcairo -ldrm -lharfbuzz \
		-lpango-1.0 -lpangocairo-1.0 -lunistring -lxcb -lxcb-cursor -lxcb-dri3 \
		-lxcb-imdkit -lxcb-keysyms -lxcb-present -lxcb-util -o $@ $<
d: d.c
	$C -I/usr/include/drm -ldrm -lxcb -lxcb-dri3 -lxcb-present -o $@ $<
rMa: ma
	./$^
ma: makeAsciiH.c
	$C -o $@ $^
