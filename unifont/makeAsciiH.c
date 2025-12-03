// Make a binary file encoding how to print the 95 glyphs 32..126
#include "i.h"
const int bPer1wide = 16; // In binary form there are 16 bytes in a 1wide glyph
const int glyH = 16;
const int hexPairPerRow = 1;
int lineWantLen = 33; // includes newline
int main(int argc, char **argv) {
  FILE *fi = eoz(fopen("u.hex", "r"));
  char line[256];
  printf("const u1t font[]="); int colN = 16;
  int is1st = 1;
  for (int lineI = 0; fgets(line, sizeof(line), fi); lineI++) {
    if (lineI < 32) continue;
    if (lineI > 126) break;
    char *p = line; while (*p && *p != ':') p++;
    p++;
    if (lineWantLen != strlen(p)) continue;
    for (int y = 0; y < glyH; y++) {
      for (int x = 0; x < hexPairPerRow; x++) {
        eoz(*p);
        eoz(p[1]);
        if (colN + 5 >= 80) {puts(""); colN = 5;} else colN += 5;
        if (is1st) {printf("{"); is1st = 0;} else printf(",");
        printf("0x%c%c", *p, p[1]);
        p += 2;
      }
    }
  }
  puts("};");
  colN = 0;
}
