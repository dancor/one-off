#include <stdio.h>
#include <stdlib.h>

int main()
{
  char *p;
  char *q;

  // Allocation #1 of 19 bytes
  //p = (char *) malloc(19);

  // Allocation #2 of 12 bytes
  p = (char *) malloc(12);

  // Allocation #3 of 16 bytes
  //p = (char *) malloc(16);

  *p = 4;
  printf("%d\n", *p);
  q = p + 12;
  *q = 5;
  printf("%d\n", *q);

  free(p);

  return 0;
}
