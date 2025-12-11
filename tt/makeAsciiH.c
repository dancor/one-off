// Make ascii.h encoding how to print the 95 glyphs 32..126
#include "i.h"
const int bPer1wide = 16; // In binary form there are 16 bytes in a 1wide glyph
const int glyH = 16;
const int hexPairPerRow = 1;
int lineWantLen = 33; // includes newline
int main(int argc, char **argv) {
  FILE *fi = eoz(fopen("u.hex", "r")), *fo = eoz(fopen("ascii.h", "w"));
  fprintf(fo, "const u1t aFont[]="); int colN = 16;
  char line[256]; int is1st = 1;
  for (int lineI = 0; fgets(line, sizeof(line), fi); lineI++) {
    if (lineI < 32) continue;
    if (lineI > 126) break;
    printf("%s", line);
    char *p = line; while (*p && *p != ':') p++;
    p++;
    if (lineWantLen != strlen(p)) continue;
    for (int y = 0; y < glyH; y++) {
      for (int x = 0; x < hexPairPerRow; x++) {
        eoz(*p);
        eoz(p[1]);
        if (colN + 5 >= 80) {fputs("\n", fo); colN = 5;} else colN += 5;
        if (is1st) {fprintf(fo, "{"); is1st = 0;} else fprintf(fo, ",");
        fprintf(fo, "0x%c%c", *p, p[1]);
        p += 2;
      }
    }
  }
  fputs("};\n", fo);
  fclose(fo);
}
