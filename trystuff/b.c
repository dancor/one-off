#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
//#include "err.h"
//#include "types.h"
// gfortran->gcc: I usually need to apply my pointers with restrict
// and __builtin_assume_aligned and my function kernels with static and 
// __attribute((always_inline))__ in order to get what I consider to be 
// optimized assembly. Allocations need to go through posix_memalign(), 
// assuming it's even available. In Fortran, I can usually just write the loop 
// without much thought.
// why restrict after if it's really int *x?: f(int* __restrict__ x)
// learn about gcc "pure" keyword?
int main(void){
  printf("hi.\n");
}
