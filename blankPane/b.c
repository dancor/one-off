#include <math.h>
#include <string.h>
#include "i.h"
#include "i/color.h"
#include "i/xwin.h"
int main(int argc, char **argv) {
  xwinOn(2560, 1440, (char*)"pbydvz");
  color_t c = (argc >= 2) ? colorRead(argv[1]) : 0;
  for (int i = 0; i < 2560 * 1440; i++) xwinP[i] = c;
  for (;;) {
    xcb_generic_event_t *e = eoz(xcb_wait_for_event(xwinC));
    if (e->response_type == XCB_EXPOSE) xwin();
  }
}
