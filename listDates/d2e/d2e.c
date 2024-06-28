#include <stdio.h>
#include <stdlib.h>
#include "../../../template/c/err.h"
#include "../../../template/c/types.h"
int curN = 120;
int readWholeFile(char **c, const char* n){ // raceCondition btwn ftell & fread
  FILE *f = nue(fopen(n, "r")); nie(fseek(f, 0, SEEK_END));
  long l = nie(ftell(f)); nie(fseek(f, 0, SEEK_SET)); *c = malloc(l);
  zie(1 == fread(*c, l, 1, f)); nze(fclose(f)); return l;}
inline void procDate(const int l, char *c, const int i){
  c[i + 11] = 0;
  // int n = 100 * (c[i + 12] - '0') + 10 * (c[i + 13] - '0') + c[i + 14] - '0'
  // + 365;
  int n = curN;
  int e3 = n % 10; n /= 10; int e2 = n % 10; n /= 10; int e1 = n % 10; n /= 10;
  printf("%s%c%i%i%i", c + i, n + 'd', e1, e2, e3);
  curN++;}
void procFile(const int l, char *c){int i = 0;
readALine:
  if (i + 15 < l &&
      c[i     ] == '2' &&
      c[i +  1] == '0' &&
      c[i +  4] == '-' &&
      c[i +  7] == '-' &&
      c[i + 10] == ' ' &&
      c[i + 11] == 'd') {procDate(l, c, i); i += 15;}
readAChar:
  if (i >= l) return;
  printf("%c", c[i]);
  if (c[i++] == '\n') goto readALine;
  goto readAChar;}
int main(){
  char *c; int l = readWholeFile(&c, "/home/danl/etc/plan/todoDanlCal.txt");
  procFile(l, c); free(c);}
