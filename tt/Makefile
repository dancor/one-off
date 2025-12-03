.phone: rF
C=gcc -O3 -Wall -Werror -ffast-math -fmax-errors=1 -fomit-frame-pointer \
	-march=native
tt: tt.c
	$C "-L$HOME/.local/lib" "-Wl,-rpath,$HOME/.local/lib" \
		-lunistring -I/usr/include/pango-1.0 -I/usr/include/libmount \
		-I/usr/include/blkid -I/usr/include/fribidi -I/usr/include/harfbuzz \
		-I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include \
		-I/usr/include/sysprof-6 -pthread -I/usr/include/cairo \
		-I/usr/include/freetype2 -I/usr/include/libpng16 -I/usr/include/pixman-1 \
		-lpangocairo-1.0 -lpango-1.0 -lharfbuzz -lgobject-2.0 -lglib-2.0 -lcairo \
		-lXext -lX11 -lXft -lfontconfig -o tt tt.c
rF: f
	./$^
f: flicker.c
	$C -o $@ $^ -lxcb
