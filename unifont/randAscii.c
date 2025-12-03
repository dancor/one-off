#include "i.h"
#include "ascii.h"
#include "i/color.h"
#include "i/xwin.h"
const int glyH = 16;
const uint32_t cWhite = 0xffFFff;
void drawN() {
  int glyRowBytes = 1, glyW = 8, glyBytes = 16;
  //if (wide) {glyCharW = 2; glyW = 16; glyBytes = 32;}
  int wX = 0, wY = 0;
  int didN = 0;
  for (;;) {
    didN++;
    int c = iRand() % 95;
    const u1t *fontP = font + glyBytes * c;
    for (int y = 0; y < glyH; y++) {
      int offset = xwinW * (wY + y) + wX;
      for (int x = 0; x < glyRowBytes; x++) {
        const u1t bits = *fontP;
        xwinP[offset++] = (bits & 128) ? cWhite : 0;
        xwinP[offset++] = (bits & 64) ? cWhite : 0;
        xwinP[offset++] = (bits & 32) ? cWhite : 0;
        xwinP[offset++] = (bits & 16) ? cWhite : 0;
        xwinP[offset++] = (bits & 8) ? cWhite : 0;
        xwinP[offset++] = (bits & 4) ? cWhite : 0;
        xwinP[offset++] = (bits & 2) ? cWhite : 0;
        xwinP[offset++] = (bits & 1) ? cWhite : 0;
        fontP++;
      }
    }
    wX += glyW;
    if (wX + glyW > xwinW) {wX = 0; wY += glyH; if (wY + glyH > xwinH) break;}
  }
  printf("didN %i\n", didN);
}
int main(int argc, char **argv) {
  iSeed();
  //const int winW = 3840, winH = 2160;
  const int winW = 640, winH = 720;
  xwinOn(winW, winH, (char*)"drawN");
  for (int i = 0; i < winW * winH; i++) xwinP[i] = 0xff0000;
  int frame = 0; 
  drawN();
  for (;;) {
    xcb_generic_event_t *e = eoz(xcb_wait_for_event(xwinC));
    if (e->response_type == XCB_EXPOSE) xwin();
    printf("frame %i\n", frame++);
  }
}
