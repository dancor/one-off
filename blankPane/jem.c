#include <math.h>
#include <string.h>
#include "../../templates/cpp/err.h"
#include "../../templates/c/xwin.h"
inline void recip(double &r2, double &i2, const double r, const double i) {
  double mag = r * r + i * i; r2 = r / mag; i2 = -i / mag;}
inline int rAb1(double x) {
  return max(0, min(255, int((x + 1) / 2 * 255 + 0.5)));
  /*
  int r = (x + 1) / 2 * 255 + 0.5;
  if (r >= 0 && r <= 255) return r;
  printf("rAb1 failed on %f\n", x); exit(1);
  */
}
int main() {
  xwinOn(2560, 1440, (char*)"pbydvz");
  bzero(xwinP, 2560 * 1440 * 4);
  int offset = 0; double r, i = 1, r2, i2; do {
    int x = 0; r = -1; do {
      recip(r2, i2, r, i);
      recip(r2, i2, i, i2);
      /*if (r2 < -1) {
        printf("r2 fail: %f\n", r2);
        exit(1);
      }
      if (i2 < -1) {
        printf("i2 fail: %f\n", i2);
        exit(1);
      }*/
      xwinP[offset++] = 256 * (256 * rAb1(r2) + 0) + rAb1(i2);
      r += 0.0007815552950371239;
    } while (x++ < 2559);
    i -= 0.001389854065323141;
  } while (offset < 2560 * 1440 - 1);
  //printf("%f %f\n", real, imag);
  for (;;) {
    auto *e = zre(xcb_wait_for_event(xwinC));
    if (e->response_type == XCB_EXPOSE) xwin();
  }
}
