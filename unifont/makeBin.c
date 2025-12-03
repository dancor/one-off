// Make a binary file encoding how to print the 95 glyphs 32..126
#include "i.h"
const int bPer1wide = 16; // In binary form there are 16 bytes in a 1wide glyph
const int glyH = 16;
const int charPerLine = 1;
int lineWantLen = 33; // includes newline
int main(int argc, char **argv) {
  FILE *fi = eoz(fopen("u.hex", "r")), *fo = eoz(fopen("font.bin", "w"));
  char line[256];
  for (int lineI = 0; fgets(line, sizeof(line), fi); lineI++) {
    if (lineI < 32) continue;
    if (lineI > 126) break;
    char *p = line; while (*p && *p != ':') p++;
    p++;
    if (lineWantLen != strlen(p)) continue;
    for (int y = 0; y < glyH; y++) {
      for (int x = 0; x < charPerLine; x++) {
        eoz(*p);
        u_char bits0 = likely(*p <= '9') ? (*p - '0') : (*p + 10 - 'A');
        eoz(*++p);
        u_char bits1 = likely(*p <= '9') ? (*p - '0') : (*p + 10 - 'A');
        //char c = 16 * bits0 + bits1;
        //eoz(1==fwrite(&c, 1, 1, fo)); p++;
        printf("%");
      }
    }
  }
  fclose(fo);
}
