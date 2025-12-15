#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <xcb/present.h>
#include <xf86drm.h>
const uint winW = 720, winH = 800;
const uint8_t bpp = 32;
uint32_t drmPitch;
void *pixBuf;
int primeFd;
uint8_t xcbDepth;
xcb_connection_t *xcbC;
xcb_window_t win;
xcb_pixmap_t pixmap;
void die(const char *msg) {perror(msg); exit(EXIT_FAILURE);}
void prepDrm() {
  int drmFd = open("/dev/dri/card1", O_RDWR | O_CLOEXEC);
  if (drmFd < 0) die("open /dev/dri/card1");
  
  struct drm_mode_create_dumb bufDat = {0};
  bufDat.width = winW;
  bufDat.height = winH;
  bufDat.bpp = bpp;
  if (drmIoctl(drmFd, DRM_IOCTL_MODE_CREATE_DUMB, &bufDat) < 0)
    die("DRM_IOCTL_MODE_CREATE_DUMB");
  drmPitch = bufDat.pitch;
  
  struct drm_mode_map_dumb map = {0};
  map.handle = bufDat.handle;
  if (drmIoctl(drmFd, DRM_IOCTL_MODE_MAP_DUMB, &map) < 0)
    die("DRM_IOCTL_MODE_MAP_DUMB");
  pixBuf = mmap(0, bufDat.size, PROT_READ | PROT_WRITE, MAP_SHARED, drmFd,
    map.offset);
  if (MAP_FAILED == pixBuf) die("mmap dumb buffer");
  
  if (drmPrimeHandleToFD(drmFd, map.handle, DRM_CLOEXEC, &primeFd) < 0)
    die("drmPrimeHandleToFD");
  
  pixmap = xcb_generate_id(xcbC);
  uint16_t stride = (uint16_t)drmPitch;
  xcb_void_cookie_t pixmapCoo = xcb_dri3_pixmap_from_buffer(xcbC, pixmap, win,
    winW * winH * bpp, winW, winH, stride, xcbDepth, bpp, primeFd);
  xcb_generic_error_t *err = xcb_request_check(xcbC, pixmapCoo);
  if (err) {
    fprintf(stderr, "DRI3 pixmap_from_buffers failed (error %d)\n",
      err->error_code);
    die("");
  }
}
uint8_t dynVal = 128;
static void fillPixBuf(void) {
  if (++dynVal < 128) return;
  uint32_t strideInPix = drmPitch / 4;
  const uint32_t green = dynVal * 256;
  for (uint32_t y = 0; y < winH; ++y) {
    uint32_t *row = (uint32_t*)pixBuf + y * strideInPix;
    for (uint32_t x = 0; x < winW; ++x) row[x] = green;
  }
}
int main(void) {
  xcbC = xcb_connect(NULL, NULL);
  if (xcb_connection_has_error(xcbC)) die("xcb_connect");
  xcb_screen_iterator_t iter = xcb_setup_roots_iterator(xcb_get_setup(xcbC));
  xcb_screen_t *xcbS = iter.data; if (!xcbS) die("cannot get default screen");
  xcbDepth = xcbS->root_depth; win = xcb_generate_id(xcbC);
  xcb_create_window(xcbC, xcbDepth, win, xcbS->root, 0, 0, winW, winH, 0,
    XCB_WINDOW_CLASS_INPUT_OUTPUT, xcbS->root_visual, XCB_CW_EVENT_MASK,
    (uint32_t[]){XCB_EVENT_MASK_EXPOSURE});
  xcb_map_window(xcbC, win); xcb_flush(xcbC);
  prepDrm();
  // If your window isn't visible at all, no need to xcb_present_pixmap().
  // When your pixels change: If you are fullscreen with no other window above
  // you, you don't need another xcb_present_pixmap() (you'll need one at the
  // start though); otherwise you do need it.
  // In times when your pixels aren't changing: You need it after an expose
  // event (idk the cost if you just did it anyway in your main loop).
  for (;;) {
    xcb_generic_event_t *e = xcb_poll_for_event(xcbC);
    fillPixBuf();
    if (e || dynVal >= 128) 
    xcb_present_pixmap(xcbC, win, pixmap, 0, 0, 0, 0, 0, 0, 0, 0,
      XCB_PRESENT_OPTION_ASYNC_MAY_TEAR, 0, 0, 0, 0, 0); xcb_flush(xcbC);
    printf("."); fflush(stdout); usleep(10*1000);
  }
}
