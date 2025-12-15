#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <xcb/present.h>
#include <xf86drm.h>
#define winW 720
#define winH 800
#define BPP 32
uint32_t drm_pitch;
void *pixBuf;
int primeFd;
uint8_t xcbDepth;
xcb_connection_t *xcbC;
xcb_window_t win;
xcb_pixmap_t pixmap;
void die(const char *msg) {
  perror(msg);
  exit(EXIT_FAILURE);
}
void prepDrm() {
  int drmFd = open("/dev/dri/card1", O_RDWR | O_CLOEXEC);
  if (drmFd < 0) die("open /dev/dri/card1");
  
  struct drm_mode_create_dumb bufDat = {0};
  bufDat.width = winW;
  bufDat.height = winH;
  bufDat.bpp = BPP;
  if (drmIoctl(drmFd, DRM_IOCTL_MODE_CREATE_DUMB, &bufDat) < 0)
    die("DRM_IOCTL_MODE_CREATE_DUMB");
  drm_pitch = bufDat.pitch;
  
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
  uint16_t stride = (uint16_t)drm_pitch;
  xcb_void_cookie_t pixmapCoo = xcb_dri3_pixmap_from_buffer(xcbC, pixmap, win,
    winW * winH * BPP, winW, winH, stride, xcbDepth, BPP, primeFd);
  xcb_generic_error_t *err = xcb_request_check(xcbC, pixmapCoo);
  if (err) {
    fprintf(stderr, "DRI3 pixmap_from_buffers failed (error %d)\n",
      err->error_code);
    die("");
  }
}
int dynVal = 128;
static void fillPixBuf(void) {
  uint32_t strideInPix = drm_pitch / 4;
  const uint32_t green = dynVal * 256;
  for (uint32_t y = 0; y < winH; ++y) {
    uint32_t *row = (uint32_t*)pixBuf + y * strideInPix;
    for (uint32_t x = 0; x < winW; ++x) row[x] = green;
  }
  dynVal = (dynVal + 1) % 256;
}
int main(void) {
  xcbC = xcb_connect(NULL, NULL);
  if (xcb_connection_has_error(xcbC)) die("xcb_connect");
  xcb_screen_iterator_t iter = xcb_setup_roots_iterator(xcb_get_setup(xcbC));
  xcb_screen_t *xcbS = iter.data;
  if (!xcbS) die("cannot get default screen");
  win = xcb_generate_id(xcbC);
  xcbDepth = xcbS->root_depth;
  xcb_create_window(xcbC, xcbDepth, win, xcbS->root, 0, 0, winW, winH, 0,
    XCB_WINDOW_CLASS_INPUT_OUTPUT, xcbS->root_visual, 0, NULL);
  xcb_map_window(xcbC, win);
  xcb_flush(xcbC);
  prepDrm();
  fillPixBuf();
  for (;;) {
    printf("."); fflush(stdout);
    fillPixBuf();
    xcb_void_cookie_t presentCoo = xcb_present_pixmap(xcbC, win, pixmap,
      0, 0, 0, 0, 0, // serial, valid, update, x, y
      0, 0, 0, // target_crtc, wait_fence, idle_fence
      XCB_PRESENT_OPTION_NONE, 0, // options, target_msc
      0, 0, 0, 0); // divisor, remainder, notifies_len, notifies
    xcb_generic_error_t *err = xcb_request_check(xcbC, presentCoo);
    if (err) {
      fprintf(stderr, "Present request failed (error %d)\n", err->error_code);
      die("");
    }
    xcb_generic_event_t *event = xcb_poll_for_event(xcbC);
    if (event) {
      switch (event->response_type & ~0x80) {
        case XCB_EXPOSE: printf("xcb_expose\n"); break;
        case XCB_CONFIGURE_NOTIFY: printf("xcb_configure_notify\n"); break;
        case XCB_CLIENT_MESSAGE: printf("xcb_client_message\n"); break;
      }
      free(event);
    }
    usleep(10*1000);
  }
}
