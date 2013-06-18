main: libmy_lib.a Main.hs
	ghc --make Main -L. -lmy_lib

libmy_lib.a: my_lib.o
	ar rcs libmy_lib.a my_lib.o

my_lib.o: my_lib.c
	gcc -g -c my_lib.c
