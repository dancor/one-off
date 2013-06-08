
PROG  = finalizer_queue

$(PROG) : FinalizerQueue.hs my_call.o
	$(HC) -threaded --make $< -o $@ -XForeignFunctionInterface my_call.o -Wall

my_call.o : my_call.c my_call.h
	gcc -c -o $@ $< -Wall

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
