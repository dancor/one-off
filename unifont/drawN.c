// ~3.6 screens of characters?
#include "i.h"
#include "i/color.h"
#include "i/xwin.h"
const int glyH = 16;
const uint32_t cWhite = 0xffFFff;
void drawN(const char *filename, int wide) {
  int glyCharW = 2, glyW = 8, slen = 33;
  if (wide) {glyCharW = 4; glyW = 16; slen = 65;}
  FILE *file = fopen(filename, "r");
  if (!file) {perror("Error opening u.hex file"); return;}
  char line[256];
  int wX = 0, wY = 0;
  int didN = 0;
  while (fgets(line, sizeof(line), file)) {
    //printf("%s", line); // includes newline
    char *p = line;
    while (*p && *p != ':') p++;
    eoz(*p++);
    if (slen != strlen(p)) continue; // includes newline
    didN++;
    //if (didN <= 32400 * 3) continue;
    for (int y = 0; y < glyH; y++) {
      int offset = xwinW * (wY + y) + wX;
      for (int x = 0; x < glyCharW; x++) {
        eoz(*p);
        u_char bits = likely(*p <= '9') ? (*p - '0') : (*p + 10 - 'A');
        xwinP[offset++] = (bits & 8) ? cWhite : 0;
        xwinP[offset++] = (bits & 4) ? cWhite : 0;
        xwinP[offset++] = (bits & 2) ? cWhite : 0;
        xwinP[offset++] = (bits & 1) ? cWhite : 0;
        p++;
      }
    }
    wX += glyW;
    if (wX + glyW > xwinW) {wX = 0; wY += glyH; if (wY + glyH > xwinH) break;}
  }
  printf("didN %i\n", didN);
}
int main(int argc, char **argv) {
  const int winW = 3840, winH = 2160;
  xwinOn(winW, winH, (char*)"drawN");
  for (int i = 0; i < winW * winH; i++) xwinP[i] = 0xff0000;
  int frame = 0; 
  drawN("u.hex", 1);
  for (;;) {
    xcb_generic_event_t *e = eoz(xcb_wait_for_event(xwinC));
    if (e->response_type == XCB_EXPOSE) xwin();
    printf("frame %i\n", frame++);
  }
}
