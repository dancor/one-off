#include <stdlib.h>
#include "my_lib.h"

int my_func(int a) {
  char *p;

  p = (char *) malloc(19);
  
  p += 20;
  *p = 4;

  return (a + 1);
}
