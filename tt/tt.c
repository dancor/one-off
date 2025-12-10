#include "i.h"
#include "ascii.h"
// for setenv(), unsetenv() & pselect() from stdlib.h:
//#define _POSIX_C_SOURCE 200112L
#include <X11/XKBlib.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <cairo/cairo-xcb.h>
#include <cairo/cairo.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <pango/pangocairo.h>
#include <pwd.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unigbrk.h>
#include <unistd.h>
#include <unistr.h>
#include <uniwidth.h>
#include <xcb-imdkit/encoding.h>
#include <xcb-imdkit/imclient.h>
#include <xcb/xcb.h>
#include <xcb/xcb_aux.h>
#include <xcb/xcb_cursor.h>
#include <xcb/xcb_keysyms.h>
#if defined(__linux)
 #include <pty.h>
#elif defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
 #include <util.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
 #include <libutil.h>
#endif
//#include "printXev.h"
#define ATTRCMP(a, b) ((a).mode!=(b).mode || (a).fg!=(b).fg || (a).bg!=(b).bg)
#define BETWEEN(x, a, b) ((a) <= (x) && (x) <= (b))
#define DEFAULT(a, b) (a) = (a) ? (a) : (b)
#define DIVCEIL(n, d) (((n) + ((d) - 1)) / (d))
#define ESC_ARG_SIZ 16
#define ESC_BUF_SIZ (128*UTF_SIZ)
#define IS_TRUECOL(x) (1 << 24 & (x))
#define LEN(a) (sizeof(a) / sizeof(a)[0])
#define LIMIT(x, a, b) (x) = (x) < (a) ? (a) : (x) > (b) ? (b) : (x)
#define MODBIT(x, set, bit) ((set) ? ((x) |= (bit)) : ((x) &= ~(bit)))
#define STR_ARG_SIZ ESC_ARG_SIZ
#define STR_BUF_SIZ ESC_BUF_SIZ
#define TIMEDIFF(t1,t2)((t1.tv_sec-t2.tv_sec)*1000+(t1.tv_nsec-t2.tv_nsec)/1E6)
#define TRUECOLOR(r,g,b) (1 << 24 | (r) << 16 | (g) << 8 | (b))
#define UTF_INVALID 0xFFFD
#define UTF_SIZ 4
#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
enum winMode {MODE_VISIBLE = 1, MODE_FOCUSED = 2, MODE_APPKEYPAD = 4,
  MODE_MOUSEBTN = 8, MODE_MOUSEMOTION = 16, MODE_REVERSE = 32, MODE_KBDLOCK =
  64,  MODE_HIDE = 128, MODE_APPCURSOR = 256, MODE_MOUSESGR = 512, MODE_8BIT =
  1024, MODE_BLINK = 2048, MODE_FBLINK = 4096, MODE_FOCUS = 8192, MODE_MOUSEX10
  = 16384, MODE_MOUSEMANY = 32768, MODE_BRCKTPASTE = 65536, MODE_NUMLOCK =
  131072,
  MODE_MOUSE = MODE_MOUSEBTN|MODE_MOUSEMOTION|MODE_MOUSEX10|MODE_MOUSEMANY};
enum glyphAttr {ATTR_NULL = 0, ATTR_BOLD = 1, ATTR_FAINT = 2, ATTR_ITALIC
  = 4, ATTR_UNDERLINE = 8, ATTR_BLINK = 16, ATTR_REVERSE = 32, ATTR_INVISIBLE =
  64, ATTR_STRUCK = 128, ATTR_WRAP = 256, ATTR_WIDE = 512, ATTR_WDUMMY = 1024,
 ATTR_BOLD_FAINT = ATTR_BOLD | ATTR_FAINT};
enum selectionMode {SEL_IDLE = 0, SEL_EMPTY = 1, SEL_READY = 2};
enum selectionType {SEL_REGULAR = 1, SEL_RECTANGULAR = 2};
enum selectionSnap {SNAP_WORD = 1, SNAP_LINE = 2};
typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned short ushort;
typedef struct {uint8_t r, g, b;} Rgb;
Rgb palette[260] = {
  {  0,  0,  0},{205,  0,  0},{  0,205,  0},{205,205,  0},
  {  0,  0,205},{205,  0,205},{  0,205,205},{229,229,229},
  {127,127,127},{255,  0,  0},{  0,255,  0},{255,255,  0},
  {  0,  0,255},{255,  0,255},{  0,255,255},{255,255,255}};
// color palette index of: foreground text, backgound, cursor, cursor reverse
int fgPalI = 0, bgPalI = 15, cuPalI = 0, crPalI = 15;
const float oneDiv255 = 0.00392156862745098;
int cmdfd;

// Block out the /usr/include/X11/extensions/render.h:29:25 Glyph:
#define Glyph Glyph_

// all utf8 sequences are <= 32byte. max attained by 95 kiss emojis
typedef struct {char u[33]; ushort mode; // character code; attribute flags;
  uint32_t fg, bg;} Glyph; typedef Glyph *Line; // foreground/background
typedef union {int i; uint ui; float f; const void *v; const char *s;} Arg;
typedef struct {KeySym k; uint mask; char *s; signed char appkey, appcursor;}
  Key; // application keypad and cursor: 0 indifferent, 1 on, -1 off
void tputc(char*); // twrite calls tputc calls tcontrolcode calls strhandle
  // calls osc_color_response calls tellShell calls twrite

// X modifiers
#define XK_ANY_MOD UINT_MAX
#define XK_NO_MOD 0
#define XK_SWITCH_MOD (1<<13|1<<14)

const uchar utfbyte[UTF_SIZ + 1] = {0x80,    0, 0xC0, 0xE0, 0xF0};
const uchar utfmask[UTF_SIZ + 1] = {0xC0, 0x80, 0xE0, 0xF0, 0xF8};

typedef struct {Atom xtarget; char *primary, *clipboard;
  struct timespec tclick1, tclick2;} XSelection; XSelection xsel;
typedef struct {xcb_connection_t *c; Display *dpy; xcb_screen_t *scr;
  cairo_surface_t *cairoSurf; cairo_t *cairo; PangoLayout *layout;
  PangoFontDescription *fontD; xcb_visualtype_t *vis;
  xcb_key_symbols_t *keysyms;
  struct {xcb_xim_t *xim; xcb_xic_t xic; xcb_point_t spot; xcb_xim_nested_list *spotlist;} ime;
  uint32_t evMask; xcb_window_t win;
  int isBold, isItalic, isfixed, l, t, gm; // is fontD set to bold, to italic,
    // is fixed geometry meaning cannot be resized by user, left and top
    // offset, geometry mask
  struct {xcb_atom_t clipboard, targets, text, utf8string,
  wmDeleteWindow, wmIconName, wmName, wmProtocols, xembed;} atom;
} XWindow; XWindow xw;
void
die(const char *errstr, ...) {
  va_list ap; va_start(ap, errstr); vfprintf(stderr, errstr, ap); va_end(ap);
  exit(1);
}
void
debug(const char *errstr, ...) {
  va_list ap; va_start(ap, errstr); vfprintf(stderr, errstr, ap); va_end(ap);
}
char*
xstrdup(const char *s) {
  char *p = strdup(s); if (!p) die("strdup: %s\n", strerror(errno));
  return p;
}
void
clipcopy(const Arg *dummy) {
  free(xsel.clipboard); xsel.clipboard = NULL;
  if (!xsel.primary) return;
  xsel.clipboard = xstrdup(xsel.primary);
  xcb_set_selection_owner(xw.c, xw.win, xw.atom.clipboard, XCB_CURRENT_TIME);
  xcb_flush(xw.c);
  xcb_get_selection_owner_cookie_t cookie = xcb_get_selection_owner(xw.c,   
    xw.atom.clipboard);
  xcb_get_selection_owner_reply_t *reply = xcb_get_selection_owner_reply(
    xw.c, cookie, NULL);
  if (!reply || reply->owner != xw.win)
    fprintf(stderr, "clipcopy: Failed to become selection owner\n");
  free(reply);
}
typedef struct {
  Glyph attr; // current char attributes
  int x, y; char state;} TCursor;
typedef struct { // internal representation of the screen
  int row; // number of rows
  int col; // number of columns
  Line *line; // screen
  Line *alt; // alternate screen
  int *dirty; // dirtyness of lines
  TCursor c; // cursor
  int ocx; // old cursor column
  int ocy; // old cursor row
  int top; // top scroll limit
  int bot; // bottom scroll limit
  int mode; // terminal mode flags
  int esc; // escape state flags
  char trantbl[4]; // charset table translation
  int charset;  // current charset
  int icharset; // selected charset for sequence
} Term; Term term;
enum termMode {MODE_WRAP=1, MODE_INSERT=2, MODE_ALTSCREEN=4, MODE_CRLF=8,
  MODE_ECHO=16, MODE_PRINT=32};
typedef struct {int mode, type, snap, alt; struct {int x, y;} nb, ne, ob, oe;
  // nb: normalized coords of beginning of selection
  // ne: normalized coords of end of selection
  // ob: original coords of beginning of selection
  // oe: original coords of end of selection
} Selection; Selection sel;
int
isBlank(const char *c) {
  return ' '==*c && !c[1];
}
void
setBlank(char *u) {
  *u = ' '; u[1] = 0;
}
int
tlinelen(int y) {
  int i = term.col; if (term.line[y][i - 1].mode & ATTR_WRAP) return i;
  while (i > 0 && isBlank(term.line[y][i - 1].u)) i--;
  return i;
}
ssize_t
xwrite(int fd, const char *s, size_t len) {
  size_t aux = len; ssize_t r;
  while (len > 0) {
    r = write(fd, s, len); if (r < 0) return r;
    len -= r; s += r;
  }
  return aux;
}
void*
xmalloc(size_t len) {
  void *p = malloc(len);
  if (!p) die("malloc: %s\n", strerror(errno));
  return p;
}
char*
getsel(void) {
  char *str, *ptr;
  int y, bufsize, lastx, linelen;
  const Glyph *gp, *last;
  if (sel.ob.x == -1) return NULL;
  bufsize = (term.col + 1) * (sel.ne.y-sel.nb.y + 1) * UTF_SIZ;
  ptr = str = xmalloc(bufsize);
  // append every set & selected glyph to the selection
  for (y = sel.nb.y; y <= sel.ne.y; y++) {
    if ((linelen = tlinelen(y)) == 0) {*ptr++ = '\n'; continue;}
    if (sel.type == SEL_RECTANGULAR) {
      gp = &term.line[y][sel.nb.x]; lastx = sel.ne.x;
    } else {
      gp = &term.line[y][sel.nb.y == y ? sel.nb.x : 0];
      lastx = sel.ne.y == y ? sel.ne.x : term.col - 1;
    }
    last = &term.line[y][MIN(lastx, linelen-1)];
    while (last >= gp && isBlank(last->u)) --last;
    for (; gp <= last; ++gp) {
      if (gp->mode & ATTR_WDUMMY) continue;
      if (likely(!gp->u[1])) *ptr++ = *gp->u;
      else {const char *gpu = gp->u; do {*ptr++ = *gpu++;} while (*gpu);}
    }
    // Copy and pasting of line endings is inconsistent in the inconsistent
    // terminal and GUI world. The best solution seems like to produce '\n'
    // when something is copied from st and convert '\n' to '\r', when
    // something to be pasted is received by st
    if ((y < sel.ne.y || lastx >= linelen) && (!(last->mode & ATTR_WRAP) ||
        sel.type == SEL_RECTANGULAR)) *ptr++ = '\n';
  }
  *ptr = 0;
  return str;
}
char *font = "Liberation Mono:pixelsize=14:antialias=true:autohint=true",
  *shell = "/bin/sh", // if not in -e nor $SHELL nor /etc/passwd
  *stty_args = "stty raw pass8 nl -echo -iexten -cstopb 38400",
  *termname = "st-256color", // $TERM
  *sayMy2027mode = "\x1b[?2027;3$y",
  *vtiden = "\x1b[?6c"; // identification sequence returned in DA and DECID
int borderpx = 0, allowaltscreen = 1,
  allowwindowops = 0, // eg allow setting clipboard text (insecure)
  bellvolume = 0; // 0 disabled. -100..100 (what is negative?)
uint blinktimeout = 800, // ms. 0 to disable. blinking attribute
  cols = 80, rows = 37,
  cursorthickness = 2, // thickness of underline and bar cursors
  doubleclicktimeout = 300, tripleclicktimeout = 600, // ms
  mousefg = 7, mousebg = 0, forcemousemod = ShiftMask;
float cwscale = 1.0, chscale = 1.0; // Kerning/boundingBox multipliers
// draw latency range: from new content/keypress/etc until drawing
// within this range, st draws when content stops arriving (idle). mostly it's
// near minlatency, but it waits longer for slow updates to avoid partial draw
// low minlatency will tear/flicker more, as it can "detect" idle too early
double minlatency = 2, maxlatency = 33; // ms
char *argv0;
#define ARGBEGIN for (argv0 = *argv, argv++, argc--; argv[0] && argv[0][0] == \
    '-' && argv[0][1]; argc--, argv++) {\
  char argc_, **argv_; int brk_;\
  if (argv[0][1] == '-' && argv[0][2] == '\0') {\
    argv++; argc--; break;}\
  int i_;\
  for (i_ = 1, brk_ = 0, argv_ = argv; argv[0][i_] && !brk_; i_++) {\
    if (argv_ != argv) break;\
    argc_ = argv[0][i_];\
    switch (argc_)
#define EARGF(x) ((argv[0][i_+1] == '\0' && argv[1] == NULL) ?\
  ((x), abort(), (char *)0) : (brk_ = 1,\
     (argv[0][i_+1] != '\0') ? (&argv[0][i_+1]) : (argc--, argv++, argv[0])))
#define ARGEND }}

// State bits to ignore when matching key or button events.  By default,
// numlock (Mod2Mask) and keyboard layout (XK_SWITCH_MOD) are ignored
uint ignoremod = Mod2Mask | XK_SWITCH_MOD;

// changing any of these breaks things with the linux world & you have to
// recompile tt.info accordingly. they are searched sequentially so XK_ANY_MOD
// must last. Use XK_ANY_MOD mask to match the key no matter modifiers state.
// Use XK_NO_MOD mask to match the key alone (no modifiers).
// key pad application mode: 0 no value, >0 enabled (2: numlock=1), <0 disabled
// cursor application mode: 0 no values, 1 enabled, -1 disabled
Key ksymsFunnyInTerms[] = {
{XK_KP_Home,ShiftMask,"\x1b[2J",0,-1},{XK_KP_Home,ShiftMask,"\x1b[1;2H",0,1},
{XK_KP_Home,XK_ANY_MOD,"\x1b[H",0,-1},{XK_KP_Home,XK_ANY_MOD,"\x1b[1~",0,1},
{XK_KP_Up,XK_ANY_MOD,"\x1bOx",1,0},{XK_KP_Up,XK_ANY_MOD,"\x1b[A",0,-1},
{XK_KP_Up,XK_ANY_MOD,"\x1bOA",0,1},{XK_KP_Down,XK_ANY_MOD,"\x1bOr",1,0},
{XK_KP_Down,XK_ANY_MOD,"\x1b[B",0,-1},{XK_KP_Down,XK_ANY_MOD,"\x1bOB",0,1},
{XK_KP_Left,XK_ANY_MOD,"\x1bOt",1,0},{XK_KP_Left,XK_ANY_MOD,"\x1b[D",0,-1},
{XK_KP_Left,XK_ANY_MOD,"\x1bOD",0,1},{XK_KP_Right,XK_ANY_MOD,"\x1bOv",1,0},
{XK_KP_Right,XK_ANY_MOD,"\x1b[C",0,-1},{XK_KP_Right,XK_ANY_MOD,"\x1bOC",0,1},
{XK_KP_Prior,ShiftMask,"\x1b[5;2~",0,0},{XK_KP_Prior,XK_ANY_MOD,"\x1b[5~",0,0},
{XK_KP_Begin,XK_ANY_MOD,"\x1b[E",0,0},{XK_KP_End,ControlMask,"\x1b[J",-1,0},
{XK_KP_End,ControlMask,"\x1b[1;5F",1,0},{XK_KP_End,ShiftMask,"\x1b[K",-1,0},
{XK_KP_End,ShiftMask,"\x1b[1;2F",1,0},{XK_KP_End,XK_ANY_MOD,"\x1b[4~",0,0},
{XK_KP_Next,ShiftMask,"\x1b[6;2~",0,0},{XK_KP_Next,XK_ANY_MOD,"\x1b[6~",0,0},
{XK_KP_Insert,ShiftMask,"\x1b[2;2~",1,0},
{XK_KP_Insert,ShiftMask,"\x1b[4l",-1,0},
{XK_KP_Insert,ControlMask,"\x1b[L",-1,0},
{XK_KP_Insert,ControlMask,"\x1b[2;5~",1,0},
{XK_KP_Insert,XK_ANY_MOD,"\x1b[4h",-1,0},
{XK_KP_Insert,XK_ANY_MOD,"\x1b[2~",1,0},
{XK_KP_Delete,ControlMask,"\x1b[M",-1,0},
{XK_KP_Delete,ControlMask,"\x1b[3;5~",1,0},
{XK_KP_Delete,ShiftMask,"\x1b[2K",-1,0},
{XK_KP_Delete,ShiftMask,"\x1b[3;2~",1,0},
{XK_KP_Delete,XK_ANY_MOD,"\x1b[P",-1,0},
{XK_KP_Delete,XK_ANY_MOD,"\x1b[3~",1,0},
{XK_KP_Multiply,XK_ANY_MOD,"\x1bOj",2,0},{XK_KP_Add,XK_ANY_MOD,"\x1bOk",2,0},
{XK_KP_Enter,XK_ANY_MOD,"\x1bOM",2,0},{XK_KP_Enter,XK_ANY_MOD,"\r",-1,0},
{XK_KP_Subtract,XK_ANY_MOD,"\x1bOm",2,0},
{XK_KP_Decimal,XK_ANY_MOD,"\x1bOn",2,0},{XK_KP_Divide,XK_ANY_MOD,"\x1bOo",2,0},
{XK_KP_0,XK_ANY_MOD,"\x1bOp",2,0},{XK_KP_1,XK_ANY_MOD,"\x1bOq",2,0},
{XK_KP_2,XK_ANY_MOD,"\x1bOr",2,0},{XK_KP_3,XK_ANY_MOD,"\x1bOs",2,0},
{XK_KP_4,XK_ANY_MOD,"\x1bOt",2,0},{XK_KP_5,XK_ANY_MOD,"\x1bOu",2,0},
{XK_KP_6,XK_ANY_MOD,"\x1bOv",2,0},{XK_KP_7,XK_ANY_MOD,"\x1bOw",2,0},
{XK_KP_8,XK_ANY_MOD,"\x1bOx",2,0},{XK_KP_9,XK_ANY_MOD,"\x1bOy",2,0},
{XK_Up,ShiftMask,"\x1b[1;2A",0,0},{XK_Up,Mod1Mask,"\x1b[1;3A",0,0},
{XK_Up,ShiftMask|Mod1Mask,"\x1b[1;4A",0,0},{XK_Up,ControlMask,"\x1b[1;5A",0,0},
{XK_Up,ShiftMask|ControlMask,"\x1b[1;6A",0,0},
{XK_Up,ControlMask|Mod1Mask,"\x1b[1;7A",0,0},
{XK_Up,ShiftMask|ControlMask|Mod1Mask,"\x1b[1;8A",0,0},
{XK_Up,XK_ANY_MOD,"\x1b[A",0,-1},{XK_Up,XK_ANY_MOD,"\x1bOA",0,1},
{XK_Down,ShiftMask,"\x1b[1;2B",0,0},{XK_Down,Mod1Mask,"\x1b[1;3B",0,0},
{XK_Down,ShiftMask|Mod1Mask,"\x1b[1;4B",0,0},
{XK_Down,ControlMask,"\x1b[1;5B",0,0},
{XK_Down,ShiftMask|ControlMask,"\x1b[1;6B",0,0},
{XK_Down,ControlMask|Mod1Mask,"\x1b[1;7B",0,0},
{XK_Down,ShiftMask|ControlMask|Mod1Mask,"\x1b[1;8B",0,0},
{XK_Down,XK_ANY_MOD,"\x1b[B",0,-1},{XK_Down,XK_ANY_MOD,"\x1bOB",0,1},
{XK_Left,ShiftMask,"\x1b[1;2D",0,0},{XK_Left,Mod1Mask,"\x1b[1;3D",0,0},
{XK_Left,ShiftMask|Mod1Mask,"\x1b[1;4D",0,0},
{XK_Left,ControlMask,"\x1b[1;5D",0,0},
{XK_Left,ShiftMask|ControlMask,"\x1b[1;6D",0,0},
{XK_Left,ControlMask|Mod1Mask,"\x1b[1;7D",0,0},
{XK_Left,ShiftMask|ControlMask|Mod1Mask,"\x1b[1;8D",0,0},
{XK_Left,XK_ANY_MOD,"\x1b[D",0,-1},{XK_Left,XK_ANY_MOD,"\x1bOD",0,1},
{XK_Right,ShiftMask,"\x1b[1;2C",0,0},{XK_Right,Mod1Mask,"\x1b[1;3C",0,0},
{XK_Right,ShiftMask|Mod1Mask,"\x1b[1;4C",0,0},
{XK_Right,ControlMask,"\x1b[1;5C",0,0},
{XK_Right,ShiftMask|ControlMask,"\x1b[1;6C",0,0},
{XK_Right,ControlMask|Mod1Mask,"\x1b[1;7C",0,0},
{XK_Right,ShiftMask|ControlMask|Mod1Mask,"\x1b[1;8C",0,0},
{XK_Right,XK_ANY_MOD,"\x1b[C",0,-1},{XK_Right,XK_ANY_MOD,"\x1bOC",0,1},
{XK_ISO_Left_Tab,ShiftMask,"\x1b[Z",0,0},{XK_Return,Mod1Mask,"\x1b\r",0,0},
{XK_Return,XK_ANY_MOD,"\r",0,0},{XK_Insert,ShiftMask,"\x1b[4l",-1,0},
{XK_Insert,ShiftMask,"\x1b[2;2~",1,0},{XK_Insert,ControlMask,"\x1b[L",-1,0},
{XK_Insert,ControlMask,"\x1b[2;5~",1,0},{XK_Insert,XK_ANY_MOD,"\x1b[4h",-1,0},
{XK_Insert,XK_ANY_MOD,"\x1b[2~",1,0},{XK_Delete,ControlMask,"\x1b[M",-1,0},
{XK_Delete,ControlMask,"\x1b[3;5~",1,0},{XK_Delete,ShiftMask,"\x1b[2K",-1,0},
{XK_Delete,ShiftMask,"\x1b[3;2~",1,0},{XK_Delete,XK_ANY_MOD,"\x1b[P",-1,0},
{XK_Delete,XK_ANY_MOD,"\x1b[3~",1,0},{XK_BackSpace,XK_NO_MOD,"\177",0,0},
{XK_BackSpace,Mod1Mask,"\x1b\177",0,0},{XK_Home,ShiftMask,"\x1b[2J",0,-1},
{XK_Home,ShiftMask,"\x1b[1;2H",0,1},{XK_Home,XK_ANY_MOD,"\x1b[H",0,-1},
{XK_Home,XK_ANY_MOD,"\x1b[1~",0,1},{XK_End,ControlMask,"\x1b[J",-1,0},
{XK_End,ControlMask,"\x1b[1;5F",1,0},{XK_End,ShiftMask,"\x1b[K",-1,0},
{XK_End,ShiftMask,"\x1b[1;2F",1,0},{XK_End,XK_ANY_MOD,"\x1b[4~",0,0},
{XK_Prior,ControlMask,"\x1b[5;5~",0,0},{XK_Prior,ShiftMask,"\x1b[5;2~",0,0},
{XK_Prior,XK_ANY_MOD,"\x1b[5~",0,0},{XK_Next,ControlMask,"\x1b[6;5~",0,0},
{XK_Next,ShiftMask,"\x1b[6;2~",0,0},{XK_Next,XK_ANY_MOD,"\x1b[6~",0,0},
{XK_F1,XK_NO_MOD,"\x1bOP",0,0},{XK_F1,ShiftMask,"\x1b[1;2P",0,0},
{XK_F1,ControlMask,"\x1b[1;5P",0,0},{XK_F1,Mod4Mask,"\x1b[1;6P",0,0},
{XK_F1,Mod1Mask,"\x1b[1;3P",0,0},{XK_F1,Mod3Mask,"\x1b[1;4P",0,0},
{XK_F2,XK_NO_MOD,"\x1bOQ",0,0},{XK_F2,ShiftMask,"\x1b[1;2Q",0,0},
{XK_F2,ControlMask,"\x1b[1;5Q",0,0},{XK_F2,Mod4Mask,"\x1b[1;6Q",0,0},
{XK_F2,Mod1Mask,"\x1b[1;3Q",0,0},{XK_F2,Mod3Mask,"\x1b[1;4Q",0,0},
{XK_F3,XK_NO_MOD,"\x1bOR",0,0},{XK_F3,ShiftMask,"\x1b[1;2R",0,0},
{XK_F3,ControlMask,"\x1b[1;5R",0,0},{XK_F3,Mod4Mask,"\x1b[1;6R",0,0},
{XK_F3,Mod1Mask,"\x1b[1;3R",0,0},{XK_F3,Mod3Mask,"\x1b[1;4R",0,0},
{XK_F4,XK_NO_MOD,"\x1bOS",0,0},{XK_F4,ShiftMask,"\x1b[1;2S",0,0},
{XK_F4,ControlMask,"\x1b[1;5S",0,0},{XK_F4,Mod4Mask,"\x1b[1;6S",0,0},
{XK_F4,Mod1Mask,"\x1b[1;3S",0,0},{XK_F5,XK_NO_MOD,"\x1b[15~",0,0},
{XK_F5,ShiftMask,"\x1b[15;2~",0,0},{XK_F5,ControlMask,"\x1b[15;5~",0,0},
{XK_F5,Mod4Mask,"\x1b[15;6~",0,0},{XK_F5,Mod1Mask,"\x1b[15;3~",0,0},
{XK_F6,XK_NO_MOD,"\x1b[17~",0,0},{XK_F6,ShiftMask,"\x1b[17;2~",0,0},
{XK_F6,ControlMask,"\x1b[17;5~",0,0},{XK_F6,Mod4Mask,"\x1b[17;6~",0,0},
{XK_F6,Mod1Mask,"\x1b[17;3~",0,0},{XK_F7,XK_NO_MOD,"\x1b[18~",0,0},
{XK_F7,ShiftMask,"\x1b[18;2~",0,0},{XK_F7,ControlMask,"\x1b[18;5~",0,0},
{XK_F7,Mod4Mask,"\x1b[18;6~",0,0},{XK_F7,Mod1Mask,"\x1b[18;3~",0,0},
{XK_F8,XK_NO_MOD,"\x1b[19~",0,0},{XK_F8,ShiftMask,"\x1b[19;2~",0,0},
{XK_F8,ControlMask,"\x1b[19;5~",0,0},{XK_F8,Mod4Mask,"\x1b[19;6~",0,0},
{XK_F8,Mod1Mask,"\x1b[19;3~",0,0},{XK_F9,XK_NO_MOD,"\x1b[20~",0,0},
{XK_F9,ShiftMask,"\x1b[20;2~",0,0},{XK_F9,ControlMask,"\x1b[20;5~",0,0},
{XK_F9,Mod4Mask,"\x1b[20;6~",0,0},{XK_F9,Mod1Mask,"\x1b[20;3~",0,0},
{XK_F10,XK_NO_MOD,"\x1b[21~",0,0},{XK_F10,ShiftMask,"\x1b[21;2~",0,0},
{XK_F10,ControlMask,"\x1b[21;5~",0,0},{XK_F10,Mod4Mask,"\x1b[21;6~",0,0},
{XK_F10,Mod1Mask,"\x1b[21;3~",0,0},{XK_F11,XK_NO_MOD,"\x1b[23~",0,0},
{XK_F11,ShiftMask,"\x1b[23;2~",0,0},{XK_F11,ControlMask,"\x1b[23;5~",0,0},
{XK_F11,Mod4Mask,"\x1b[23;6~",0,0},{XK_F11,Mod1Mask,"\x1b[23;3~",0,0},
{XK_F12,XK_NO_MOD,"\x1b[24~",0,0},{XK_F12,ShiftMask,"\x1b[24;2~",0,0},
{XK_F12,ControlMask,"\x1b[24;5~",0,0},{XK_F12,Mod4Mask,"\x1b[24;6~",0,0},
{XK_F12,Mod1Mask,"\x1b[24;3~",0,0},{XK_F13,XK_NO_MOD,"\x1b[1;2P",0,0},
{XK_F14,XK_NO_MOD,"\x1b[1;2Q",0,0},{XK_F15,XK_NO_MOD,"\x1b[1;2R",0,0},
{XK_F16,XK_NO_MOD,"\x1b[1;2S",0,0},{XK_F17,XK_NO_MOD,"\x1b[15;2~",0,0},
{XK_F18,XK_NO_MOD,"\x1b[17;2~",0,0},{XK_F19,XK_NO_MOD,"\x1b[18;2~",0,0},
{XK_F20,XK_NO_MOD,"\x1b[19;2~",0,0},{XK_F21,XK_NO_MOD,"\x1b[20;2~",0,0},
{XK_F22,XK_NO_MOD,"\x1b[21;2~",0,0},{XK_F23,XK_NO_MOD,"\x1b[23;2~",0,0},
{XK_F24,XK_NO_MOD,"\x1b[24;2~",0,0},{XK_F25,XK_NO_MOD,"\x1b[1;5P",0,0},
{XK_F26,XK_NO_MOD,"\x1b[1;5Q",0,0},{XK_F27,XK_NO_MOD,"\x1b[1;5R",0,0},
{XK_F28,XK_NO_MOD,"\x1b[1;5S",0,0},{XK_F29,XK_NO_MOD,"\x1b[15;5~",0,0},
{XK_F30,XK_NO_MOD,"\x1b[17;5~",0,0},{XK_F31,XK_NO_MOD,"\x1b[18;5~",0,0},
{XK_F32,XK_NO_MOD,"\x1b[19;5~",0,0},{XK_F33,XK_NO_MOD,"\x1b[20;5~",0,0},
{XK_F34,XK_NO_MOD,"\x1b[21;5~",0,0},{XK_F35,XK_NO_MOD,"\x1b[23;5~",0,0}};
// Selection types' masks. Use the same masks as usual.
// Button1Mask is always unset, to make masks match between ButtonPress.
// ButtonRelease and MotionNotify.
// If no match is found, regular selection is used
uint selmasks[] = {[SEL_RECTANGULAR] = Mod1Mask};

// macros
#define T_IS_SET(flag) ((term.mode & (flag)) != 0)
#define ISCONTROL(c) (BETWEEN(c, 0, 0x1f) || (c) == 0x7f)

enum cursor_movement {CURSOR_SAVE, CURSOR_LOAD};
enum cursor_state {CURSOR_DEFAULT, CURSOR_WRAPNEXT, CURSOR_ORIGIN};
enum charset {CS_GRAPHIC0, CS_GRAPHIC1, CS_UK, CS_USA, CS_MULTI, CS_GER,
  CS_FIN};
enum escape_state {ESC_START = 1, ESC_CSI = 2,
  ESC_STR = 4, // DCS, OSC, PM, APC
  ESC_ALTCHARSET = 8,
  ESC_STR_END = 16, // a final string was encountered
  ESC_TEST = 32, // Enter in test mode
  ESC_UTF8 = 64};
// control sequence introducer terminal escape sequence
// ESC '[' [[ [<priv>] <arg> [;]] <mode> [<mode>]]
typedef struct {
  char buf[ESC_BUF_SIZ]; // everything after '[' and before ESC '\\'
  size_t len; // buf length
  char priv; // 1 when the mode is specified as private
  int arg[ESC_ARG_SIZ]; // parsed int arguments
  int narg; // number of arguments
  char mode[2];
} CsiEscape; CsiEscape csiEsc;
// "string" terminal escape sequence eg setWindowTitle "\e]2;My Title\e\\"
// ESC type [[[<priv>] <arg> [;]] <mode>] ESC '\\'
typedef struct {
  char type; // command class eg ]=operatingSystemCommand P=deviceControlString
  char *buf; // everything after type and before ESC '\\'
  size_t len; // buf used length
  char *args[STR_ARG_SIZ]; // pointers to the arguments in buf
  int narg; // number of arguments
} StrEscape; StrEscape strEsc;

char **opt_cmd  = NULL, *opt_class = NULL, *opt_embed = NULL, *opt_line = NULL,
  *opt_name = NULL, *opt_title = NULL;
uint buttons; // bit field of pressed buttons

typedef struct {size_t collen; GC gc;} DC; DC dc; // Drawing Context
typedef struct { // Purely graphic info
  int tw, th, // tty width and height
  w, h, // window width and height
  ch, // char height
  cw, // char width
  mode, // window state/mode flags
  cursor; // cursor style
} TermWindow; TermWindow win;
pid_t pid;

void *
xrealloc(void *p, size_t len) {
  if ((p = realloc(p, len)) == NULL) die("realloc: %s\n", strerror(errno));
  return p;
}
char
base64dec_getc(const char **src) {
  while (**src && !isprint((unsigned char)**src)) (*src)++;
  return **src ? *((*src)++) : '=';  // emulate padding if string ends
}
char*
base64dec(const char *src) {
  size_t in_len = strlen(src);
  char *result, *dst;
  static const char base64_digits[256] = {
    [43] = 62, 0, 0, 0, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
    0, 0, 0, -1, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0, 0, 0, 0,
    0, 0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
  };

  if (in_len % 4) in_len += 4 - (in_len % 4);
  result = dst = xmalloc(in_len / 4 * 3 + 1);
  while (*src) {
    int a = base64_digits[(unsigned char) base64dec_getc(&src)];
    int b = base64_digits[(unsigned char) base64dec_getc(&src)];
    int c = base64_digits[(unsigned char) base64dec_getc(&src)];
    int d = base64_digits[(unsigned char) base64dec_getc(&src)];

    // invalid input. 'a' can be -1, e.g. if src is "\n" (c-str)
    if (a == -1 || b == -1) break;

    *dst++ = (a << 2) | ((b & 0x30) >> 4);
    if (c == -1) break;
    *dst++ = ((b & 0x0f) << 4) | ((c & 0x3c) >> 2);
    if (d == -1) break;
    *dst++ = ((c & 0x03) << 6) | d;
  }
  *dst = '\0';
  return result;
}
void
selinit(void) {
  sel.mode = SEL_IDLE; sel.snap = 0; sel.ob.x = -1;
}
void
selsnap(int *x, int *y, int direction) {
  int newx, newy, xt, yt;
  int delim, prevdelim;
  const Glyph *gp, *prevgp;

  switch (sel.snap) {
  case SNAP_WORD:
    // Snap around if the word wraps around at the end or beginning of a line
    prevgp = &term.line[*y][*x];
    prevdelim = isBlank(prevgp->u);
    for (;;) {
      newx = *x + direction;
      newy = *y;
      if (!BETWEEN(newx, 0, term.col - 1)) {
        newy += direction;
        newx = (newx + term.col) % term.col;
        if (!BETWEEN(newy, 0, term.row - 1)) break;
        if (direction > 0) yt = *y, xt = *x; else yt = newy, xt = newx;
        if (!(term.line[yt][xt].mode & ATTR_WRAP)) break;
      }

      if (newx >= tlinelen(newy)) break;

      gp = &term.line[newy][newx];
      delim = isBlank(gp->u);
      if (!(gp->mode & ATTR_WDUMMY) && (delim != prevdelim ||
          (delim && gp->u != prevgp->u))) break;

      *x = newx; *y = newy; prevgp = gp; prevdelim = delim;
    }
    break;
  case SNAP_LINE:
    // Snap around if the the previous line or the current one has set
    // ATTR_WRAP at its end. Then the whole next or previous line will be
    // selected.
    *x = direction < 0 ? 0 : term.col - 1;
    if (direction < 0) {
      for (; *y > 0; *y += direction)
        if (!(term.line[*y - 1][term.col - 1].mode & ATTR_WRAP)) break;
    } else if (direction > 0) {
      for (; *y < term.row - 1; *y += direction)
        if (!(term.line[*y][term.col - 1].mode & ATTR_WRAP)) break;
    }
    break;
  }
}
void
selnormalize(void) {
  if (sel.type == SEL_REGULAR && sel.ob.y != sel.oe.y) {
    sel.nb.x = sel.ob.y < sel.oe.y ? sel.ob.x : sel.oe.x;
    sel.ne.x = sel.ob.y < sel.oe.y ? sel.oe.x : sel.ob.x;
  } else {
    sel.nb.x = MIN(sel.ob.x, sel.oe.x);
    sel.ne.x = MAX(sel.ob.x, sel.oe.x);
  }
  sel.nb.y = MIN(sel.ob.y, sel.oe.y);
  sel.ne.y = MAX(sel.ob.y, sel.oe.y);
  selsnap(&sel.nb.x, &sel.nb.y, -1);
  selsnap(&sel.ne.x, &sel.ne.y, +1);
  // expand selection over line breaks
  if (sel.type == SEL_RECTANGULAR) return;
  int i = tlinelen(sel.nb.y);
  if (i < sel.nb.x) sel.nb.x = i;
  if (tlinelen(sel.ne.y) <= sel.ne.x) sel.ne.x = term.col - 1;
}
void
tsetdirt(int top, int bot) {
  LIMIT(top, 0, term.row - 1); LIMIT(bot, 0, term.row - 1);
  for (int i = top; i <= bot; i++) term.dirty[i] = 1;
}
void
selclear(void) {
  if (sel.ob.x == -1) return;
  sel.mode = SEL_IDLE; sel.ob.x = -1; tsetdirt(sel.nb.y, sel.ne.y);
}
void
selstart(int col, int row, int snap) {
  selclear(); sel.mode = SEL_EMPTY; sel.type = SEL_REGULAR;
  sel.alt = T_IS_SET(MODE_ALTSCREEN); sel.snap = snap;
  sel.oe.x = sel.ob.x = col; sel.oe.y = sel.ob.y = row; selnormalize();
  if (sel.snap != 0) sel.mode = SEL_READY;
  tsetdirt(sel.nb.y, sel.ne.y);
}
void
selextend(int col, int row, int type, int done) {
  if (sel.mode == SEL_IDLE) return;
  if (done && sel.mode == SEL_EMPTY) {selclear(); return;}
  int oldex = sel.oe.x, oldey = sel.oe.y, oldsby = sel.nb.y, oldsey = sel.ne.y,
    oldtype = sel.type;
  sel.oe.x = col; sel.oe.y = row; selnormalize(); sel.type = type;
  if (oldey != sel.oe.y || oldex != sel.oe.x || oldtype != sel.type || sel.mode
    == SEL_EMPTY) tsetdirt(MIN(sel.nb.y, oldsby), MAX(sel.ne.y, oldsey));
  sel.mode = done ? SEL_IDLE : SEL_READY;
}
int
selected(int x, int y) {
  if (likely(sel.mode == SEL_EMPTY || sel.ob.x == -1 ||
      sel.alt != T_IS_SET(MODE_ALTSCREEN))) return 0;
  if (unlikely(sel.type == SEL_RECTANGULAR)) return
    BETWEEN(y, sel.nb.y, sel.ne.y) && BETWEEN(x, sel.nb.x, sel.ne.x);
  return BETWEEN(y, sel.nb.y, sel.ne.y) && (y != sel.nb.y || x >= sel.nb.x) &&
    (y != sel.ne.y || x <= sel.ne.x);
}
void
execsh(char *cmd, char **args) {
  char *sh, *prog, *arg; const struct passwd *pw;
  errno = 0;
  if (!(pw = getpwuid(getuid()))) {
    if (errno) die("getpwuid: %s\n", strerror(errno));
    else die("who are you?\n");
  }
  if (!(sh = getenv("SHELL"))) sh = (pw->pw_shell[0]) ? pw->pw_shell : cmd;
  if (args) {prog = args[0]; arg = NULL;} else {prog = sh; arg = NULL;}
  DEFAULT(args, ((char *[]) {prog, arg, NULL}));
  unsetenv("COLUMNS"); unsetenv("LINES"); unsetenv("TERMCAP");
  setenv("LOGNAME", pw->pw_name, 1); setenv("USER", pw->pw_name, 1);
  setenv("SHELL", sh, 1); setenv("HOME", pw->pw_dir, 1);
  setenv("TERM", termname, 1);
  signal(SIGCHLD, SIG_DFL); signal(SIGHUP, SIG_DFL); signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL); signal(SIGTERM, SIG_DFL); signal(SIGALRM, SIG_DFL);
  execvp(prog, args); _exit(1);
}
void
sigchld(int a) {
  int stat; pid_t p;
  if ((p = waitpid(pid, &stat, WNOHANG)) < 0)
    die("waiting for pid %hd failed: %s\n", pid, strerror(errno));
  if (pid != p) return;
  if (WIFEXITED(stat) && WEXITSTATUS(stat))
    die("child exited with status %d\n", WEXITSTATUS(stat));
  else if (WIFSIGNALED(stat))
    die("child terminated due to signal %d\n", WTERMSIG(stat));
  _exit(0);
}
void
stty(char **args) {
  char cmd[_POSIX_ARG_MAX], **p, *q, *s; size_t n, siz;
  if ((n = strlen(stty_args)) > sizeof(cmd) - 1)
    die("incorrect stty parameters\n");
  memcpy(cmd, stty_args, n);
  q = cmd + n;
  siz = sizeof(cmd) - n;
  for (p = args; p && (s = *p); ++p) {
    if ((n = strlen(s)) > siz - 1) die("stty parameter length too long\n");
    *q++ = ' ';
    memcpy(q, s, n);
    q += n;
    siz -= n + 1;
  }
  *q = '\0';
  if (system(cmd) != 0) perror("Couldn't call stty");
}
int
ttynew(const char *line, char *cmd, char **args) {
  if (line) {
    if ((cmdfd = open(line, O_RDWR)) < 0)
      die("open line '%s' failed: %s\n", line, strerror(errno));
    dup2(cmdfd, 0);
    stty(args);
    return cmdfd;
  }
  // seems to work fine on linux, openbsd and freebsd
  int m, s; if (openpty(&m, &s, NULL, NULL, NULL) < 0)
    die("openpty failed: %s\n", strerror(errno));

  switch (pid = fork()) {
  case -1: die("fork failed: %s\n", strerror(errno)); break;
  case 0:
    close(m);
    setsid(); // create a new process group
    dup2(s, 0); dup2(s, 1); dup2(s, 2);
    if (ioctl(s, TIOCSCTTY, NULL) < 0)
      die("ioctl TIOCSCTTY failed: %s\n", strerror(errno));
    if (s > 2) close(s);
#ifdef __OpenBSD__
    if (pledge("stdio getpw proc exec", NULL) == -1) die("pledge\n");
#endif
    execsh(cmd, args); break;
  default:
#ifdef __OpenBSD__
    if (pledge("stdio rpath tty proc", NULL) == -1) die("pledge\n");
#endif
    close(s); cmdfd = m; signal(SIGCHLD, sigchld); break;
  }
  return cmdfd;
}
void
tmoveto(int x, int y) {
  int miny, maxy;
  if (term.c.state & CURSOR_ORIGIN) {miny = term.top; maxy = term.bot;}
  else {miny = 0; maxy = term.row - 1;}
  term.c.state &= ~CURSOR_WRAPNEXT;
  term.c.x = LIMIT(x, 0, term.col - 1); term.c.y = LIMIT(y, miny, maxy);
}
void
tclearregion(int x1, int y1, int x2, int y2) {
  int x, y, tmp; Glyph *gp;
  if (x1 > x2) tmp = x1, x1 = x2, x2 = tmp;
  if (y1 > y2) tmp = y1, y1 = y2, y2 = tmp;
  LIMIT(x1, 0, term.col - 1); LIMIT(y1, 0, term.row - 1);
  LIMIT(x2, 0, term.col - 1); LIMIT(y2, 0, term.row - 1);
  for (y = y1; y <= y2; y++) {
    term.dirty[y] = 1;
    for (x = x1; x <= x2; x++) {
      gp = &term.line[y][x];
      if (selected(x, y)) selclear();
      gp->fg = term.c.attr.fg; gp->bg = term.c.attr.bg;
      gp->mode = 0; setBlank(gp->u);
    }
  }
}
void
selscroll(int orig, int n) {
  if (sel.ob.x == -1 || sel.alt != T_IS_SET(MODE_ALTSCREEN)) return;
  if (BETWEEN(sel.nb.y, orig, term.bot) != BETWEEN(sel.ne.y, orig, term.bot)) {
    selclear();
  } else if (BETWEEN(sel.nb.y, orig, term.bot)) {
    sel.ob.y += n; sel.oe.y += n;
    if (sel.ob.y < term.top || sel.ob.y > term.bot ||
        sel.oe.y < term.top || sel.oe.y > term.bot) selclear();
    else selnormalize();
  }
}
void
tscrollup(int orig, int n) {
  int i; Line tmp; LIMIT(n, 0, term.bot-orig+1);
  tclearregion(0, orig, term.col - 1, orig + n - 1);
  tsetdirt(orig+n, term.bot);
  for (i = orig; i <= term.bot-n; i++) {
    tmp = term.line[i]; term.line[i] = term.line[i+n]; term.line[i+n] = tmp;
  }
  selscroll(orig, -n);
}
void
tnewline(int andGoX0) {
  int y = term.c.y; if (y == term.bot) tscrollup(term.top, 1); else y++;
  tmoveto(andGoX0 ? 0 : term.c.x, y);
}
void
strparse(void) {
  int c; char *p = strEsc.buf;
  strEsc.narg = 0;
  strEsc.buf[strEsc.len] = '\0';
  if (*p == '\0') return;
  while (strEsc.narg < STR_ARG_SIZ) {
    strEsc.args[strEsc.narg++] = p;
    while ((c = *p) != ';' && c != '\0') ++p;
    if (c == '\0') return;
    *p++ = '\0';
  }
}
void
xsettitle(char *p) {
  DEFAULT(p, opt_title); if (!*p) p = opt_title;
  xcb_change_property(xw.c, XCB_PROP_MODE_REPLACE, xw.win, xw.atom.wmName,
    xw.atom.utf8string, 8, strlen(p), p);
}
void
xseticontitle(char *p) {
  DEFAULT(p, opt_title); if (!*p) p = opt_title;
  xcb_change_property(xw.c, XCB_PROP_MODE_REPLACE, xw.win,
    xw.atom.wmIconName, xw.atom.utf8string, 8, strlen(p), p);
}
void
setsel(char *str, Time t) {
  if (!str) return;
  free(xsel.primary); xsel.primary = str;
  xcb_set_selection_owner(xw.c, xw.win, XCB_ATOM_PRIMARY, t);
  xcb_flush(xw.c);
  xcb_get_selection_owner_cookie_t cookie = xcb_get_selection_owner(xw.c,   
    XCB_ATOM_PRIMARY);
  xcb_get_selection_owner_reply_t *reply = xcb_get_selection_owner_reply(
    xw.c, cookie, NULL);
  if (!reply || reply->owner != xw.win)
    fprintf(stderr, "setsel: Failed to become selection owner\n");
  free(reply);
}
// Send each complete unicode codepoint of buf to tputc(), which will process
// control characters (which we also first trigger printing as eg ^A if
// showEscCtrl) and print non-control codepoints. We determine & blindly trust
// the bytesize of the codepoint from the upper bits of the 1st byte. We return
// n to indicate how much of buf we used: For a final partial codepoint in buf,
// we leave that unprocessed to be put in the start of buf for the next call to
// twrite().
int
twrite(const char *buf, int bufLen, int showEscCtrl) {
  int n = 0; while (n < bufLen) {
    uchar c = buf[n];
    if (likely(c < 128)) {
      if (showEscCtrl && ISCONTROL(c)) {
        if (c & 0x80) {c &= 0x7f; tputc("^"); tputc("[");}
        else if (c != '\n' && c != '\r' && c != '\t') {c ^= 0x40; tputc("^");}
      }
      char u[2] = {c, 0};
      tputc(u); n++; continue;
    }
    if (likely(c < 224)) { // 2byte utf8
      if (unlikely(n + 1 >= bufLen)) break; // saves partial for next twrite
      char u[3] = {buf[n], buf[n + 1], 0};
      tputc(u); n += 2; continue;
    }
    if (likely(c < 240)) { // 3byte utf8
      if (unlikely(n + 2 >= bufLen)) break;
      char u[4] = {buf[n], buf[n + 1], buf[n + 2], 0}; tputc(u); n += 3;
      continue;
    }
    if (unlikely(n + 3 >= bufLen)) break;
    char u[5] = {buf[n], buf[n + 1], buf[n + 2], buf[n + 3], 0}; tputc(u);
    n += 4;
  }
  return n;
}
// Take some output, up to BUFSIZ (8192 on my system) bytes, from the shell
// command process and send it to twrite() to process that output appropriately
// (to process escape sequences, update the terminal state, place new graphemes
// and reprint dirty terminal lines).
size_t
hearShell(void) {
  static char buf[BUFSIZ]; static int buflen = 0; int ret, written;
  // append read bytes to unprocessed bytes
  ret = read(cmdfd, buf + buflen, BUFSIZ - buflen);
  switch (ret) {
  case 0: exit(0);
  case -1: die("couldn't read from shell: %s\n", strerror(errno));
  default:
    buflen += ret; written = twrite(buf, buflen, 0); buflen -= written;
    // Keep any incomplete utf8 byte sequence for the next call.
    if (buflen > 0) memmove(buf, buf + written, buflen);
    return ret;
  }
}
void
tellShellRaw(const char *s, size_t n) {
  fd_set wfd, rfd; ssize_t r; size_t lim = 256;
  // Remember that we are using a pty, which might be a modem line. Writing too
  // much will clog the line. That's why we are doing this dance.
  while (n > 0) {
    FD_ZERO(&wfd); FD_ZERO(&rfd); FD_SET(cmdfd, &wfd); FD_SET(cmdfd, &rfd);
    // Check if we can write
    if (pselect(cmdfd + 1, &rfd, &wfd, NULL, NULL, NULL) < 0) {
      if (errno == EINTR) continue;
      die("select failed: %s\n", strerror(errno));
    }
    if (FD_ISSET(cmdfd, &wfd)) {
      // Only write the bytes written by tellShell() or the default of 256.
      // This seems to be a reasonable value for a serial line. Bigger values
      // might clog the I/O.
      if (unlikely((r = write(cmdfd, s, (n < lim)? n : lim)) < 0)) 
        die("write error on tty: %s\n", strerror(errno));
      if (r < n) {
        // We weren't able to write out everything. This means the buffer is
        // getting full again. Empty it
        if (n < lim) lim = hearShell();
        n -= r;
        s += r;
      } else break; // All bytes have been written.
    }
    if (FD_ISSET(cmdfd, &rfd)) lim = hearShell();
  }
  return;
}
void
tellShell(const char *s, size_t n, int mayEcho) {
  const char *next;
  if (mayEcho && T_IS_SET(MODE_ECHO)) twrite(s, n, 1);
  if (!T_IS_SET(MODE_CRLF)) {tellShellRaw(s, n); return;}
  while (n > 0) { // This is similar to how the kernel handles ONLCR for ttys
    if (*s == '\r') {
      next = s + 1;
      tellShellRaw("\r\n", 2);
    } else {
      next = memchr(s, '\r', n);
      DEFAULT(next, s + n);
      tellShellRaw(s, next - s);
    }
    n -= next - s;
    s = next;
  }
}
void
osc_color_response(int num, int index, int is_osc4) {
  int n, palI = is_osc4 ? num : index; char buf[32];
  if (palI < 0 || palI >= 256) {
    fprintf(stderr, "erresc: failed to fetch %s color %d\n",
      is_osc4 ? "osc4" : "osc", is_osc4 ? num : index); return;
  }
  Rgb c = palette[palI];
  n = snprintf(buf, sizeof buf, "\x1b]%s%d;rgb:%02x%02x/%02x%02x/%02x%02x\007",
    is_osc4 ? "4;" : "", num, c.r, c.r, c.g, c.g, c.b, c.b);
  if (n < 0 || n >= sizeof(buf)) {
    fprintf(stderr, "error: %s while printing %s response\n", n < 0 ?
      "snprintf failed" : "truncation occurred", is_osc4 ? "osc4" : "osc");
  } else tellShell(buf, n, 1);
}
void
tfulldirt(void) {
  tsetdirt(0, term.row - 1);
}
void
strdump(void) {
  fprintf(stderr, "ESC%c", strEsc.type);
  for (size_t i = 0; i < strEsc.len; i++) {
    uint c = strEsc.buf[i] & 0xff;
    if (c == '\0') {putc('\n', stderr); return;}
    else if (isprint(c)) putc(c, stderr);
    else if (c == '\n') fprintf(stderr, "(\\n)");
    else if (c == '\r') fprintf(stderr, "(\\r)");
    else if (c == 0x1b) fprintf(stderr, "(\\e)");
    else fprintf(stderr, "(%02x)", c);
  }
  fprintf(stderr, "ESC\\\n");
}
void
strhandle(void) {
  //debug("str["); strdump(); debug("]\n");
  char *dec;
  int narg, par;
  int j;
  char *p = NULL;
  //int j, narg, par;
  const struct {int idx; char *str;} osc_table[] = {
    {fgPalI, "foreground"},
    {bgPalI, "background"},
    {fgPalI, "cursor"}
  };
  term.esc &= ~(ESC_STR_END|ESC_STR);
  strparse();
  par = (narg = strEsc.narg) ? atoi(strEsc.args[0]) : 0;

  switch (strEsc.type) {
  case ']': // OSC: Operating System Command
    switch (par) {
    case 0:
      if (narg > 1) {xsettitle(strEsc.args[1]); xseticontitle(strEsc.args[1]);}
      return;
    case 1: if (narg > 1) xseticontitle(strEsc.args[1]);
      return;
    case 2: if (narg > 1) xsettitle(strEsc.args[1]);
      return;
    case 52:
      if (narg > 2 && allowwindowops) {
        dec = base64dec(strEsc.args[2]);
        if (dec) {setsel(dec, CurrentTime); clipcopy(NULL);}
        else fprintf(stderr, "erresc: invalid base64\n");
      }
      return;
    case 10: case 11: case 12: if (narg < 2) break;
      p = strEsc.args[1];
      if ((j = par - 10) < 0 || j >= LEN(osc_table)) break; // unpossible
      if (!strcmp(p, "?")) {
        //printf("strhandle case 12 j=%i p=[%s]\n", j, p);
        osc_color_response(par, osc_table[j].idx, 0);
      } else printf("strhandle case 12 pNotQmark j=%i p=[%s]\n", j, p);
      /*
      } else if (xsetcolorname(osc_table[j].idx, p)) {
        fprintf(stderr, "erresc: invalid %s color: %s\n", osc_table[j].str, p);
      } else tfulldirt();
      */
      return;
    case 4:
      if (narg < 3) break;
      p = strEsc.args[2];
      printf("strhandle case 4 p=[%s]\n", p);
      // color set; FALLTHROUGH
    case 104: // color reset
      printf("strhandle case 104\n");
      j = (narg > 1) ? atoi(strEsc.args[1]) : -1;
      if (p && !strcmp(p, "?")) osc_color_response(j, 0, 1);
      /*
      else if (xsetcolorname(j, p)) {
        if (par == 104 && narg <= 1) {
          xloadcols();
          return; // color reset without parameter
        }
        fprintf(stderr, "erresc: invalid color j=%d, p=%s\n", j, p?p:"(null)");
      } else {
        // TODO if defaultbg color is changed, borders are dirty
        tfulldirt();
      }*/
      return;
    }
    break;
  case 'k': // old title set compatibility
    xsettitle(strEsc.args[0]); return;
  case 'P': // DCS: Device Control String
  case '_': // APC: Application Program Command
  case '^': // PM: Privacy Message
    return;
  }
  fprintf(stderr, "erresc: unknown str "); strdump();
}
#define IS_SET(flag) ((win.mode & (flag)) != 0)
#define TRUERED(x) (((x) & 0xff0000) >> 16)
#define TRUEGREEN(x) (((x) & 0xff00) >> 8)
#define TRUEBLUE(x) ((x) & 0xff)

void
csireset(void) {
  memset(&csiEsc, 0, sizeof(csiEsc));
}
void
tsetchar(const char *u, const Glyph *attr, int x, int y) {
  //printf("tsetchar(%s) %li\n", u, strlen(u));
  if (term.line[y][x].mode & ATTR_WIDE) {
    if (x + 1 < term.col) {
      setBlank(term.line[y][x + 1].u);
      term.line[y][x + 1].mode &= ~ATTR_WDUMMY;
    }
  } else if (term.line[y][x].mode & ATTR_WDUMMY) {
    setBlank(term.line[y][x - 1].u);
    term.line[y][x - 1].mode &= ~ATTR_WIDE;
  }
  term.line[y][x] = *attr;
  term.dirty[y] = 1;
  strcpy(term.line[y][x].u, u);
}
void
strreset(void) {
  strEsc = (StrEscape){.buf = xrealloc(strEsc.buf, STR_BUF_SIZ)};
}
void
tstrsequence(uchar c) {
  switch (c) {
  case 0x90: c = 'P'; break; // DCS: Device Control String
  case 0x9f: c = '_'; break; // APC: Application Program Command
  case 0x9e: c = '^'; break; // PM: Privacy Message
  case 0x9d: c = ']'; break; // OSC: Operating System Command
  }
  strreset();
  strEsc.type = c;
  term.esc |= ESC_STR;
}
void
tcontrolcode(uchar ascii) {
  switch (ascii) {
  case '\b': tmoveto(term.c.x - 1, term.c.y); return; // backspace
  case '\x1b': csireset(); term.esc &= ~(ESC_CSI|ESC_ALTCHARSET|ESC_TEST);
    term.esc |= ESC_START; return; // ESC
  case '\n': case '\f': case '\v': tnewline(T_IS_SET(MODE_CRLF)); return;
  case '\t': term.c.x = MIN((term.c.x + 8) / 8 * 8, term.col - 1); return;
  case '\r': tmoveto(0, term.c.y); return; // carriage return
  case '\a': if (term.esc & ESC_STR_END) {strhandle();} break;
  case 14: case 15: term.charset = 1 - (ascii - 14); return; // locking shift
  case 26: tsetchar("?", &term.c.attr, term.c.x, term.c.y); // SUB; FALLTHROUGH
  case 24: csireset(); break; // CAN
  case 133: tnewline(1); break; // NEL: Next line: always go to first col
  case 154: // DECID: Identify Terminal
          tellShell(vtiden, strlen(vtiden), 0); break;
  case 159: tstrsequence(ascii); return; // APC: Application Program Command
  case 0: case 5: case 17: case 19: case 177: return; // NUL/ENQ/XON/XOFF/DEL
  }
  // only CAN or SUB or \a interrupt a sequence
  term.esc &= ~(ESC_STR_END|ESC_STR);
}
void
csiparse(void) {
  char *p = csiEsc.buf, *np, sep = ';'; long int v; csiEsc.narg = 0;
  if (*p == '?') {csiEsc.priv = 1; p++;}
  for (csiEsc.buf[csiEsc.len] = '\0'; p < csiEsc.buf + csiEsc.len; p++) {
    np = NULL; v = strtol(p, &np, 10); if (np == p) v = 0;
    if (v == LONG_MAX || v == LONG_MIN) v = -1;
    csiEsc.arg[csiEsc.narg++] = v; p = np;
    if (sep == ';' && *p == ':') sep = ':'; // allow override to colon once
    if (*p != sep || csiEsc.narg == ESC_ARG_SIZ) break;
  }
  csiEsc.mode[0] = *p++;
  csiEsc.mode[1] = (p < csiEsc.buf + csiEsc.len) ? *p : '\0';
}
void
csidump(void) {
  size_t i; uint c;
  fprintf(stderr, "ESC[");
  for (i = 0; i < csiEsc.len; i++) {
    c = csiEsc.buf[i] & 0xff;
    if (isprint(c)) putc(c, stderr);
    else if (c == '\n') fprintf(stderr, "(\\n)");
    else if (c == '\r') fprintf(stderr, "(\\r)");
    else if (c == 0x1b) fprintf(stderr, "(\\e)");
    else fprintf(stderr, "(%02x)", c);
  }
  putc('\n', stderr);
}
void
tinsertblank(int n) {
  int dst, src, size; Glyph *line;
  LIMIT(n, 0, term.col - term.c.x);
  dst = term.c.x + n;
  src = term.c.x;
  size = term.col - dst;
  line = term.line[term.c.y];
  memmove(&line[dst], &line[src], size * sizeof(Glyph));
  tclearregion(src, term.c.y, dst - 1, term.c.y);
}
// for absolute user moves, when decom is set
void
tmoveato(int x, int y) {
  tmoveto(x, y + ((term.c.state & CURSOR_ORIGIN) ? term.top: 0));
}
void
tscrolldown(int orig, int n) {
  int i;
  Line tmp;
  LIMIT(n, 0, term.bot-orig+1);
  tsetdirt(orig, term.bot-n);
  tclearregion(0, term.bot-n+1, term.col - 1, term.bot);
  for (i = term.bot; i >= orig+n; i--) {
    tmp = term.line[i]; term.line[i] = term.line[i-n]; term.line[i-n] = tmp;
  }
  selscroll(orig, n);
}
void
tinsertblankline(int n) {
  if (BETWEEN(term.c.y, term.top, term.bot)) tscrolldown(term.c.y, n);
}
void
xclear(void) {
  Rgb b = palette[bgPalI];
  cairo_set_source_rgb(xw.cairo, b.r*oneDiv255, b.g*oneDiv255, b.b*oneDiv255);
  cairo_rectangle(xw.cairo, 0, 0, win.w, win.h);
}
void
xPrintUtf8seg(char *utf8, int x1, int xOver, int y, uint32_t fg, uint32_t bg,
    int isBold, int isItalic, int width) {
  //printf("utf8seg[%s]\n", utf8);
  Rgb f, b; if (likely(!IS_TRUECOL(fg))) f = palette[fg];
  else {f.r = TRUERED(fg); f.g = TRUEGREEN(fg); f.b = TRUEBLUE(fg);}
  if (likely(!IS_TRUECOL(bg))) b = palette[bg];
  else {b.r = TRUERED(bg); b.g = TRUEGREEN(bg); b.b = TRUEBLUE(bg);}
  int rectX = borderpx + x1 * win.cw, rectY = borderpx + y * win.ch,
      rectW = width * win.cw;
  cairo_save(xw.cairo);
  cairo_set_source_rgb(xw.cairo, b.r*oneDiv255, b.g*oneDiv255, b.b*oneDiv255);
  cairo_rectangle(xw.cairo, rectX, rectY, rectW, win.ch);
  cairo_fill(xw.cairo);
  cairo_rectangle(xw.cairo, rectX, rectY, rectW, win.ch);
  cairo_clip(xw.cairo);
  cairo_set_source_rgb(xw.cairo, f.r*oneDiv255, f.g*oneDiv255, f.b*oneDiv255);
  pango_layout_set_text(xw.layout, utf8, -1);
  if (unlikely(isBold && !xw.isBold)) {xw.isBold = 1;
    pango_font_description_set_weight(xw.fontD, PANGO_WEIGHT_BOLD);}
  if (unlikely(!isBold && xw.isBold)) {xw.isBold = 0;
    pango_font_description_set_weight(xw.fontD, PANGO_WEIGHT_NORMAL);}
  if (unlikely(isItalic && !xw.isItalic)) {xw.isItalic = 1;
    pango_font_description_set_style(xw.fontD, PANGO_STYLE_ITALIC);}
  if (unlikely(!isItalic && xw.isItalic)) {xw.isItalic = 0;
    pango_font_description_set_style(xw.fontD, PANGO_STYLE_NORMAL);}
  pango_layout_set_font_description(xw.layout, xw.fontD);
  int textH; pango_layout_get_size(xw.layout, NULL, &textH); textH /= 1024;
  cairo_move_to(xw.cairo, rectX, rectY + win.ch/2 - textH/2);
  //cairo_move_to(xw.cairo, rectX, rectY);
  pango_cairo_show_layout(xw.cairo, xw.layout);
  cairo_restore(xw.cairo);
}
/* I didn't add these; could: 
  if ((base.mode & ATTR_BOLD_FAINT) == ATTR_BOLD && BETWEEN(base.fg, 0, 7))
    fg = &dc.col[base.fg + 8]; // Change basic colors 0-7 to bright 8-15
  if ((base.mode & ATTR_BOLD_FAINT) == ATTR_FAINT) {
    colfg.red = fg->color.red / 2;
    colfg.green = fg->color.green / 2;
    colfg.blue = fg->color.blue / 2;
    colfg.alpha = fg->color.alpha;
    XftColorAllocValue(xw.dpy, xw.vis, xw.cmap, &colfg, &revfg);
    fg = &revfg;
  }
  if (base.mode & ATTR_INVISIBLE) fg = bg;
  if (base.mode & ATTR_BLINK && win.mode & MODE_BLINK) fg = bg;
*/
void
xPrintLine(int x, int xOver, int y, Glyph g) // assumes xOver > x1
{
  //printf("xPrintLine x=%i xOver=%i y=%i g.mode=%i term.line[y][x].mode=%i\n",
  //  x, xOver, y, g.mode, term.line[y][x].mode);
  /*for (int yy = 0; yy < 2; yy++) {
    printf("line %d 0..9:[", yy);
    for (int xx = 0; xx < 9; xx++) {
      printf("%s,", term.line[yy][xx].u);
    }
    printf("]\n");
  }*/
  // A segment is a contiguous row of characters with the same attributes so
  // we can send them to cairo all at once. Since xOver > x1 we know we'll print
  // at least one segment. x and g are the start column and attributes for the
  // current segment utf8 accumulates the utf8 we'll print for the current
  // segment.
  /*const uint8_t*s = (const uint8_t*)utf8Str,
  *end = s + strlen(utf8Str), *p = s;
  printf("doU[%s]\n", utf8Str);
  int graphemeCount = 0;
  while (p < end) {
    const uint8_t *nextP = u8_grapheme_next(p,end);
    int bytes = nextP - p;
    printf("grapheme:%i bytes:%zu width:%i val[%.*s]\n", graphemeCount++,
      bytes, u8_width(p, bytes, ""), bytes, (const char*)p);
    p = nextP;
  }*/
  char utf8[2048], *utf8p = utf8; int xCur = x + 1, gMode = g.mode,
    gIsBold = (gMode & ATTR_BOLD) > 0, gIsItalic = (gMode & ATTR_ITALIC) > 0,
    gIsWide = (gMode & ATTR_WIDE) > 0;
  uint32_t gFg = g.fg, gBg = g.bg;
  if (likely(!g.u[1])) *utf8p++ = *g.u;
  else {const char *gu = g.u; do {*utf8p++ = *gu++;} while (*gu);}
  if (selected(x, y)) gMode ^= ATTR_REVERSE;
  if (gMode & ATTR_REVERSE) {gFg = g.bg; gBg = g.fg;}
  Line line = term.line[y];
  int width = unlikely(gIsWide) ? 2 : 1;
  for (;; xCur++) {
    if (unlikely(xCur >= xOver)) { // print the final segment
      *utf8p = 0; xPrintUtf8seg(utf8, x, xOver, y, gFg, gBg, gIsBold,
        gIsItalic, width); return;
    }
    Glyph gCur = line[xCur];
    // skip the dummy spacer after a wide character
    if (unlikely(gCur.mode & ATTR_WDUMMY)) continue;
    int gCurMode = gCur.mode, gCurIsBold = (gCurMode & ATTR_BOLD) > 0,
        gCurIsItalic = (gCurMode & ATTR_ITALIC) > 0,
        gCurIsWide = (gCurMode & ATTR_WIDE) > 0;
    if (selected(xCur, y)) gCurMode ^= ATTR_REVERSE;
    uint32_t gCurFg = gCur.fg, gCurBg = gCur.bg;
    if (gCurMode & ATTR_REVERSE) {gCurFg = gCur.bg; gCurBg = gCur.fg;}
    //if (unlikely(gFg != gCurFg || gBg != gCurBg || gIsBold != gCurIsBold ||
    //    gIsItalic != gCurIsItalic /*|| gIsWide != gCurIsWide*/)) {
    if (1) { // arabic throws off heights; fix this way but lose connections
      // print x..xCur-1 as a segment since xCur starts a new segment
      *utf8p = 0; xPrintUtf8seg(utf8, x, xCur, y, gFg, gBg, gIsBold,
        gIsItalic, width);
      utf8p = utf8; x = xCur; g = gCur; gFg = gCurFg; gBg = gCurBg;
      gIsBold = gCurIsBold; gIsItalic = gCurIsItalic; gMode = gCurMode;
      /*gIsWide = gCurIsWide;*/ width = 0; //unlikely(gIsWide) ? 2 : 1;
    }
    // the usual case is the current segment simply grows
    if (likely(!gCur.u[1])) *utf8p++ = *gCur.u;
    else {const char *gcu = gCur.u; do {*utf8p++ = *gcu++;} while (*gcu);}
    width += unlikely(gCurIsWide) ? 2 : 1;
  }
}
void
drawregion(int x1, int y1, int x2, int y2) {
  for (int y = y1; y < y2; y++) {
    if (!term.dirty[y]) continue;
    term.dirty[y] = 0; xPrintLine(x1, x2, y, term.line[y][x1]);
  }
}
void
xdrawcursor(int cx, int cy, int ox, int oy) {
  Glyph og = term.line[oy][ox];
  if (selected(ox, oy)) og.mode ^= ATTR_REVERSE;
  xPrintLine(ox, ox + 1, oy, og); // remove old cursor
  if (IS_SET(MODE_HIDE)) return;
  // Select the right color for the right mode
  Glyph g = term.line[cy][cx];
  g.mode &= ATTR_BOLD|ATTR_ITALIC|ATTR_UNDERLINE|ATTR_STRUCK|ATTR_WIDE;
  Rgb c; c.r = c.g = c.b = 0;
  if (IS_SET(MODE_REVERSE)) {
    g.mode |= ATTR_REVERSE; g.bg = fgPalI;
    if (selected(cx, cy)) {c = palette[cuPalI]; g.fg = crPalI;}
    else {c = palette[crPalI]; g.fg = cuPalI;}
  } else {
    if (selected(cx, cy)) {g.fg = fgPalI; g.bg = crPalI;}
    else {g.fg = bgPalI; g.bg = cuPalI;}
  }
  cairo_set_source_rgb(xw.cairo, c.r*oneDiv255, c.g*oneDiv255, c.b*oneDiv255);
  if (IS_SET(MODE_FOCUSED)) {
    switch (win.cursor) {
    case 0: // Blinking Block
    case 1: // Blinking Block (Default)
    case 2: xPrintLine(cx, cx + 1, cy, g); return; // Steady Block
    case 3: // Blinking Underline
    case 4: // Steady Underline; TODO: make double wide if g.mode WIDE?
      cairo_rectangle(xw.cairo, borderpx + cx * win.cw, borderpx +
        (cy + 1) * win.ch - cursorthickness, win.cw, cursorthickness); break;
    case 5: // Blinking bar
    case 6: // Steady bar
      cairo_rectangle(xw.cairo, borderpx + cx * win.cw, borderpx + cy * win.ch,
        cursorthickness, win.ch); break;
    }
    cairo_fill(xw.cairo); return;
  }
  cairo_rectangle(xw.cairo, borderpx + cx * win.cw,
    borderpx + cy * win.ch, win.cw - 1, 1); cairo_fill(xw.cairo);
  cairo_rectangle(xw.cairo, borderpx + cx * win.cw,
    borderpx + cy * win.ch, 1, win.ch - 1); cairo_fill(xw.cairo);
  cairo_rectangle(xw.cairo, borderpx + (cx + 1) * win.cw - 1,
    borderpx + cy * win.ch, 1, win.ch - 1); cairo_fill(xw.cairo);
  cairo_rectangle(xw.cairo, borderpx + cx * win.cw,
    borderpx + (cy + 1) * win.ch - 1, win.cw, 1); cairo_fill(xw.cairo);
}
/*void
xximspot(int x, int y) {
  if (!xw.ime.xic) return;
  xw.ime.spot.x = borderpx + x * win.cw;
  xw.ime.spot.y = borderpx + (y + 1) * win.ch;
  XSetICValues(xw.ime.xic, XNPreeditAttributes, xw.ime.spotlist, NULL);
}*/

void
draw(void) {
  int cx = term.c.x;
  //int cx = term.c.x, ocx = term.ocx, ocy = term.ocy;
  if (!IS_SET(MODE_VISIBLE)) return;
  LIMIT(term.ocx, 0, term.col - 1); // adjust cursor position
  LIMIT(term.ocy, 0, term.row - 1);
  if (term.line[term.ocy][term.ocx].mode & ATTR_WDUMMY) term.ocx--;
  if (term.line[term.c.y][cx].mode & ATTR_WDUMMY) cx--;
  drawregion(0, 0, term.col, term.row);
  xdrawcursor(cx, term.c.y, term.ocx, term.ocy);
  term.ocx = cx; term.ocy = term.c.y;
  // if do double-buffer:
  //XCopyArea(xw.dpy, xw.pbuf, xw.win, dc.gc, 0, 0, win.w, win.h, 0, 0);
  //if (ocx != term.ocx || ocy != term.ocy) xximspot(term.ocx, term.ocy);
}
void
redraw(void) {
  tfulldirt(); draw();
}
void
xsetmode(int set, unsigned int flags) {
  int mode = win.mode;
  MODBIT(win.mode, set, flags);
  if ((win.mode & MODE_REVERSE) != (mode & MODE_REVERSE)) redraw();
}
void
xsetpointermotion(int set) {
  MODBIT(xw.evMask, set, PointerMotionMask);
  xcb_change_window_attributes(xw.c, xw.win, XCB_CW_EVENT_MASK,
    (const uint32_t[]){xw.evMask});
}
void
tcursor(int mode) {
  static TCursor c[2]; int alt = T_IS_SET(MODE_ALTSCREEN);
  if (mode == CURSOR_SAVE) c[alt] = term.c;
  else if (mode == CURSOR_LOAD) {
    term.c = c[alt];
    tmoveto(c[alt].x, c[alt].y);
  }
}
void
tswapscreen(void) {
  Line *tmp = term.line; term.line = term.alt; term.alt = tmp;
  term.mode ^= MODE_ALTSCREEN;
  tfulldirt();
}
void
tsetmode(int priv, int set, const int *args, int narg) {
  int alt; const int *lim;

  for (lim = args + narg; args < lim; ++args) {
    if (priv) {
      switch (*args) {
      case 1: xsetmode(set, MODE_APPCURSOR); break; // DECCKM: Cursor key
      case 5: xsetmode(set, MODE_REVERSE); break; // DECSCNM: Reverse video
      case 6: MODBIT(term.c.state, set, CURSOR_ORIGIN); tmoveato(0, 0); break;
        // DECOM: Origin
      case 7: MODBIT(term.mode, set, MODE_WRAP); break; // DECAWM: Auto wrap
      case 0:  // Error (IGNORED)
      case 2:  // DECANM: ANSI/VT52 (IGNORED)
      case 3:  // DECCOLM: Column  (IGNORED)
      case 4:  // DECSCLM: Scroll (IGNORED)
      case 8:  // DECARM: Auto repeat (IGNORED)
      case 18: // DECPFF: Printer feed (IGNORED)
      case 19: // DECPEX: Printer extent (IGNORED)
      case 42: // DECNRCM: National characters (IGNORED)
      case 12: break; // att610: Start blinking cursor (IGNORED)
      case 25: // DECTCEM: Text Cursor Enable Mode
        xsetmode(!set, MODE_HIDE); break;
      case 9: // X10 mouse compatibility mode
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEX10);
        break;
      case 1000: // 1000: report button press
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEBTN);
        break;
      case 1002: // 1002: report motion on button press
        xsetpointermotion(0);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEMOTION);
        break;
      case 1003: // 1003: enable all mouse motions
        xsetpointermotion(set);
        xsetmode(0, MODE_MOUSE);
        xsetmode(set, MODE_MOUSEMANY);
        break;
      case 1004: // 1004: send focus events to tty
        xsetmode(set, MODE_FOCUS); break;
      case 1006: // 1006: extended reporting mode
        xsetmode(set, MODE_MOUSESGR); break;
      case 1034: xsetmode(set, MODE_8BIT); break;
      case 1049: // swap screen & set/restore cursor as xterm
        if (!allowaltscreen) break;
        tcursor((set) ? CURSOR_SAVE : CURSOR_LOAD); // FALLTHROUGH
      case 47: // swap screen
      case 1047:
        if (!allowaltscreen) break;
        alt = T_IS_SET(MODE_ALTSCREEN);
        if (alt) tclearregion(0, 0, term.col - 1, term.row - 1);
        if (set ^ alt) tswapscreen(); // set is always 1 or 0
        if (*args != 1049) break; // FALLTHROUGH
      case 1048: tcursor(set ? CURSOR_SAVE : CURSOR_LOAD); break;
      case 2004: // 2004: bracketed paste mode
        xsetmode(set, MODE_BRCKTPASTE); break;
      // Not implemented mouse modes. See comments there.
      case 1001: // mouse highlight mode; can hang the terminal by design when
                 // implemented
      case 1005: // utf8 mouse mode; will confuse applications not supporting
                 // utf8 and luit
      case 1015: // urxvt mangled mouse mode; incompatible and can be mistaken
                 // for other control codes
        break;
      default:
        fprintf(stderr, "erresc: unknown private set/reset mode %d\n", *args);
        break;
      }
    } else {
      switch (*args) {
      case 0: break; // Error (IGNORED)
      case 2: xsetmode(set, MODE_KBDLOCK); break;
      case 4: // IRM: Insertion-replacement
        MODBIT(term.mode, set, MODE_INSERT); break;
      case 12: // SRM: Send/Receive
        MODBIT(term.mode, !set, MODE_ECHO); break;
      case 20: // LNM: Linefeed/new line
        MODBIT(term.mode, set, MODE_CRLF); break;
      default:
        fprintf(stderr, "erresc: unknown set/reset mode %d\n", *args); break;
      }
    }
  }
}
void
tdeleteline(int n) {
  if (BETWEEN(term.c.y, term.top, term.bot)) tscrollup(term.c.y, n);
}
void
tdeletechar(int n) {
  int dst, src, size; Glyph *line; LIMIT(n, 0, term.col - term.c.x);
  dst = term.c.x; src = term.c.x + n; size = term.col - src;
  line = term.line[term.c.y];
  memmove(&line[dst], &line[src], size * sizeof(Glyph));
  tclearregion(term.col - n, term.c.y, term.col - 1, term.c.y);
}
// Returns the color.
int32_t
tdefcolor(const int *attr, int *npar, int l) {
  int32_t idx = -1; uint r, g, b;
  switch (attr[*npar + 1]) {
  case 2: // direct color in RGB space
    if (*npar + 4 >= l) {
      fprintf(stderr, "erresc(38): Incorrect number of parameters (%d)\n",
        *npar);
      break;
    }
    r = attr[*npar + 2]; g = attr[*npar + 3]; b = attr[*npar + 4]; *npar += 4;
    if (!BETWEEN(r, 0, 255) || !BETWEEN(g, 0, 255) || !BETWEEN(b, 0, 255))
      fprintf(stderr, "erresc: bad rgb color (%u,%u,%u)\n", r, g, b);
    else {
      idx = TRUECOLOR(r, g, b);
      //printf("tdefcolor=%d\n", idx);
    }
    break;
  case 5: // indexed color
    if (*npar + 2 >= l) {
      fprintf(stderr, "erresc(38): Incorrect number of parameters (%d)\n",
        *npar); break;
    }
    *npar += 2;
    if (!BETWEEN(attr[*npar], 0, 255))
      fprintf(stderr, "erresc: bad fgcolor %d\n", attr[*npar]);
    else idx = attr[*npar];
    break;
  case 0: // implemented defined (only foreground)
  case 1: // transparent
  case 3: // direct color in CMY space
  case 4: // direct color in CMYK space
  default: fprintf(stderr, "erresc(38): gfx attr %d unknown\n", attr[*npar]);
    break;
  }
  return idx;
}
void
tsetattr(const int *attr, int l) {
  int32_t color;
  for (int i = 0; i < l; i++) switch (attr[i]) {
  case 0:
    term.c.attr.mode &= ~(ATTR_BOLD | ATTR_FAINT | ATTR_ITALIC |
      ATTR_UNDERLINE | ATTR_BLINK | ATTR_REVERSE | ATTR_INVISIBLE |
      ATTR_STRUCK);
    term.c.attr.fg = fgPalI; term.c.attr.bg = bgPalI; break;
  case 1: term.c.attr.mode |= ATTR_BOLD; break;
  case 2: term.c.attr.mode |= ATTR_FAINT; break;
  case 3: term.c.attr.mode |= ATTR_ITALIC; break;
  case 4: term.c.attr.mode |= ATTR_UNDERLINE; break;
  case 5: // slow blink; FALLTHROUGH
  case 6: term.c.attr.mode |= ATTR_BLINK; break; // rapid blink
  case 7: term.c.attr.mode |= ATTR_REVERSE; break;
  case 8: term.c.attr.mode |= ATTR_INVISIBLE; break;
  case 9: term.c.attr.mode |= ATTR_STRUCK; break;
  case 22: term.c.attr.mode &= ~(ATTR_BOLD | ATTR_FAINT); break;
  case 23: term.c.attr.mode &= ~ATTR_ITALIC; break;
  case 24: term.c.attr.mode &= ~ATTR_UNDERLINE; break;
  case 25: term.c.attr.mode &= ~ATTR_BLINK; break;
  case 27: term.c.attr.mode &= ~ATTR_REVERSE; break;
  case 28: term.c.attr.mode &= ~ATTR_INVISIBLE; break;
  case 29: term.c.attr.mode &= ~ATTR_STRUCK; break;
  case 38: if ((color = tdefcolor(attr, &i, l)) >= 0) term.c.attr.fg = color;
    break;
  case 39: term.c.attr.fg = fgPalI; break;
  case 48: if ((color = tdefcolor(attr, &i, l)) >= 0) term.c.attr.bg = color;
    break;
  case 49: term.c.attr.bg = bgPalI; break;
  default:
    if (BETWEEN(attr[i], 30, 37)) term.c.attr.fg = attr[i] - 30;
    else if (BETWEEN(attr[i], 40, 47)) term.c.attr.bg = attr[i] - 40;
    else if (BETWEEN(attr[i], 90, 97)) term.c.attr.fg = attr[i] - 90 + 8;
    else if (BETWEEN(attr[i], 100, 107)) term.c.attr.bg = attr[i] - 100 + 8;
    else {
      fprintf(stderr, "erresc(default): gfx attr %d unknown\n", attr[i]);
      csidump();}
    break;
  }
}
void
tsetscroll(int t, int b) {
  LIMIT(t, 0, term.row - 1); LIMIT(b, 0, term.row - 1);
  if (t > b) {int tmp = t; t = b; b = tmp;}
  term.top = t; term.bot = b;
}
int
xsetcursor(int cursor) {
  if (likely(BETWEEN(cursor, 0, 6))) {win.cursor = cursor; return 0;}
  return 1;
}
void
csihandle(void) {
  //debug(stderr, "csi["); csidump(); debug(stderr, "]\n");
  char buf[40]; int len;
  switch (csiEsc.mode[0]) {
  case '@': // ICH: Insert <n> blank char
    DEFAULT(csiEsc.arg[0], 1);
    tinsertblank(csiEsc.arg[0]);
    return;
  case 'A': // CUU: Cursor <n> Up
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(term.c.x, term.c.y-csiEsc.arg[0]);
    return;
  case 'B': // CUD: Cursor <n> Down
  case 'e': // VPR --Cursor <n> Down
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(term.c.x, term.c.y+csiEsc.arg[0]);
    return;
  case 'i': // MC: Media Copy
    switch (csiEsc.arg[0]) {
    case 4: term.mode &= ~MODE_PRINT;
    case 5: term.mode |= MODE_PRINT;
    }
    return;
  case 'c': // DA: Device Attributes
    if (csiEsc.arg[0] == 0) tellShell(vtiden, strlen(vtiden), 0);
    return;
  case 'C': // CUF: Cursor <n> Forward
  case 'a': // HPR: Cursor <n> Forward
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(term.c.x + csiEsc.arg[0], term.c.y);
    return;
  case 'D': // CUB: Cursor <n> Backward
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(term.c.x - csiEsc.arg[0], term.c.y);
    return;
  case 'E': // CNL: Cursor <n> Down and first col
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(0, term.c.y + csiEsc.arg[0]);
    return;
  case 'F': // CPL: Cursor <n> Up and first col
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(0, term.c.y - csiEsc.arg[0]);
    return;
  case 'G': case '`':
    DEFAULT(csiEsc.arg[0], 1);
    tmoveto(csiEsc.arg[0] - 1, term.c.y);
    return;
  case 'H': case 'f':
    DEFAULT(csiEsc.arg[0], 1);
    DEFAULT(csiEsc.arg[1], 1);
    tmoveato(csiEsc.arg[1] - 1, csiEsc.arg[0] - 1);
    return;
  case 'J': // ED: Clear screen
    switch (csiEsc.arg[0]) {
    case 0: // below
      tclearregion(term.c.x, term.c.y, term.col - 1, term.c.y);
      if (term.c.y < term.row - 1) {
        tclearregion(0, term.c.y + 1, term.col - 1, term.row-1);
      }
      return;
    case 1: // above
      if (term.c.y > 0) tclearregion(0, 0, term.col - 1, term.c.y - 1);
      tclearregion(0, term.c.y, term.c.x, term.c.y);
      return;
    case 2: // all
      tclearregion(0, 0, term.col - 1, term.row - 1);
      return;
    }
  case 'K': // EL: Clear line
    switch (csiEsc.arg[0]) {
    case 0: // right
      tclearregion(term.c.x, term.c.y, term.col - 1, term.c.y); return;
    case 1: // left
      tclearregion(0, term.c.y, term.c.x, term.c.y); return;
    case 2: // all
      tclearregion(0, term.c.y, term.col - 1, term.c.y); return;
    }
    return;
  case 'S': // SU: Scroll <n> line up
    if (csiEsc.priv) return;
    DEFAULT(csiEsc.arg[0], 1);
    tscrollup(term.top, csiEsc.arg[0]);
    return;
  case 'T': // SD: Scroll <n> line down
    DEFAULT(csiEsc.arg[0], 1);
    tscrolldown(term.top, csiEsc.arg[0]);
    return;
  case 'L': // IL: Insert <n> blank lines
    DEFAULT(csiEsc.arg[0], 1);
    tinsertblankline(csiEsc.arg[0]);
    return;
  case 'l': // RM: Reset Mode
    tsetmode(csiEsc.priv, 0, csiEsc.arg, csiEsc.narg);
    return;
  case 'M': // DL: Delete <n> lines
    DEFAULT(csiEsc.arg[0], 1);
    tdeleteline(csiEsc.arg[0]);
    return;
  case 'X': // ECH: Erase <n> char
    DEFAULT(csiEsc.arg[0], 1);
    tclearregion(term.c.x, term.c.y, term.c.x + csiEsc.arg[0]-1, term.c.y);
    return;
  case 'P': // DCH: Delete <n> char
    DEFAULT(csiEsc.arg[0], 1);
    tdeletechar(csiEsc.arg[0]);
    return;
  case 'd': // VPA: Move to <row>
    DEFAULT(csiEsc.arg[0], 1);
    tmoveato(term.c.x, csiEsc.arg[0]-1);
    return;
  case 'h': // SM: Set terminal mode
    tsetmode(csiEsc.priv, 1, csiEsc.arg, csiEsc.narg);
    return;
  case 'm': // SGR: Terminal attribute (color)
    tsetattr(csiEsc.arg, csiEsc.narg);
    return;
  case 'n': // DSR: Device Status Report
    switch (csiEsc.arg[0]) {
    case 5: // Status Report "OK" `0n`
      tellShell("\x1b[0n", sizeof("\x1b[0n") - 1, 0);
      return;
    case 6: // Report Cursor Position (CPR) "<row>;<column>R"
      len = snprintf(buf, sizeof(buf), "\x1b[%i;%iR", term.c.y+1, term.c.x+1);
      tellShell(buf, len, 0);
      return;
    }
  case '$':
    if ('p' == csiEsc.mode[1] && csiEsc.arg[0] == 2027) {
      //printf("sayMy2027mode[%s]\n", sayMy2027mode);
      tellShell(sayMy2027mode, strlen(sayMy2027mode), 0); return;
    }
  case 'r': // DECSTBM: Set Scrolling Region
    if (!csiEsc.priv) {
      DEFAULT(csiEsc.arg[0], 1);
      DEFAULT(csiEsc.arg[1], term.row);
      tsetscroll(csiEsc.arg[0]-1, csiEsc.arg[1]-1);
      tmoveato(0, 0);
      return;
    }
  case 's': tcursor(CURSOR_SAVE); return;
  case 'u': if (!csiEsc.priv) {tcursor(CURSOR_LOAD); return;}
  case ' ': if ('q' == csiEsc.mode[1] && !xsetcursor(csiEsc.arg[0])) return;
  case 'I': case 'Z': case 'b': case 'g': return;
  }
  fprintf(stderr, "erresc: unknown csi "); csidump();
}
void
treset(void) {
  term.c = (TCursor){{.mode = ATTR_NULL, .fg = fgPalI, .bg = bgPalI},
    .x = 0, .y = 0, .state = CURSOR_DEFAULT};
  term.top = 0; term.bot = term.row - 1; term.mode = MODE_WRAP;
  memset(term.trantbl, CS_USA, sizeof(term.trantbl)); term.charset = 0;
  for (uint i = 0; i < 2; i++) {tmoveto(0, 0); tcursor(CURSOR_SAVE);
    tclearregion(0, 0, term.col - 1, term.row - 1); tswapscreen();
  }
}
// returns 1 when the sequence is finished and it hasn't to read more
// characters for this sequence, otherwise 0
int
eschandle(uchar ascii) {
  switch (ascii) {
  case '[': term.esc |= ESC_CSI; return 0;
  case '#': term.esc |= ESC_TEST; return 0;
  case '%': term.esc |= ESC_UTF8; return 0;
  case 'P': // DCS: Device Control String
  case '_': // APC: Application Program Command
  case '^': // PM: Privacy Message
  case ']': // OSC: Operating System Command
  case 'k': tstrsequence(ascii); return 0; // old title set compatibility
  case 'n': // LS2: Locking shift 2
  case 'o': term.charset = 2 + (ascii - 'n'); break; // LS3: Locking shift 3
  case '(': // GZD4: set primary charset G0
  case ')': // G1D4: set secondary charset G1
  case '*': // G2D4: set tertiary charset G2
  case '+': // G3D4: set quaternary charset G3
    term.icharset = ascii - '('; term.esc |= ESC_ALTCHARSET; return 0;
  case 'D': // IND: Linefeed
    if (term.c.y == term.bot) tscrollup(term.top, 1);
    else tmoveto(term.c.x, term.c.y+1);
    break;
  case 'E': tnewline(1); break; // NEL: Next line; always go to first col
  case 'H': break; // HTS: Horizontal tab stop
  case 'M': // RI: Reverse index
    if (term.c.y == term.top) tscrolldown(term.top, 1);
    else tmoveto(term.c.x, term.c.y-1);
    break;
  case 'Z': // DECID: Identify Terminal
    tellShell(vtiden, strlen(vtiden), 0); break;
  case 'c': // RIS: Reset to initial state
    treset(); xsettitle(NULL); xsetmode(0, MODE_HIDE); break;
  case '=': xsetmode(1, MODE_APPKEYPAD); break; // DECPAM: Application keypad
  case '>': xsetmode(0, MODE_APPKEYPAD); break; // DECPNM: Normal keypad
  case '7': tcursor(CURSOR_SAVE); break; // DECSC: Save Cursor
  case '8': tcursor(CURSOR_LOAD); break; // DECRC: Restore Cursor
  case '\\': if (term.esc & ESC_STR_END) strhandle(); // ST: String Terminator
    break;
  default: fprintf(stderr, "erresc: unknown sequence ESC 0x%02X '%c'\n",
      (uchar)ascii, isprint(ascii) ? ascii : '.'); break;
  }
  return 1;
}
int
tryCompose(char *a, char *b) {
  char *a0 = a + strlen(a), *aP = a0, *bP = b;
  do {
    if (aP >= a + 32) {
      *a0 = '\0';
      printf("noRoom2Compose(%s,%s) %li + %li %li\n",
        a, b, strlen(a), strlen(b), aP - a);
      return 0; // no room to compose
    }
    *aP++ = *bP++;
  } while (*bP);
  *aP = '\0';
  if (unlikely(aP == (char*)u8_grapheme_next((uchar*)a, (uchar*)aP))) {
    //printf("DID COMPOSE [%s]", a);
    return 1;
  }
  *a0 = '\0';
  //printf("noCompose(%s,%s)\n", a, b);
  return 0;
}
// The utf8 sequence u is 1 codepoint. If it's a control character, deal with
// it correctly. If we start, or are in, or end an escape sequence, deal with
// that correctly (or correctly enough; I ignore a lot of old stuff to keep it
// lean). Otherwise, put the codepoint in the terminal's current-position cell:
// - If appending it to the current content of the cell still forms 1
//   unicode grapheme cluster, do that. Unless this would overflow the number
//   of bytes it can hold (in which case do whatever's easy; which is?).
//   Don't worry about validating improper unicode etc; stay lean.
// - Otherwise, u replaces the cell contents.
// We are taking this appending-based approach. The only other real option is
// to have some time-based cutoff, but that seems harder to implement, to
// possibly have worse latency, and to invite the possibility of disjoining a
// grapheme when there is any stutter in the transmission of the bytes.
//
// tputc() is only called by twrite().
void
tputc(char *u) {
  /*// debugging
  printf("term.esc=%i y=%i x=%i tputc(", term.esc, term.c.y, term.c.x);
  for (int i = 0; u[i]; i++) {
    printf("%i", u[i]);
    if (u[i] >= '0' && u[i] <= 'z') printf("=%c", u[i]);
    printf(",");
  }
  printf(")\n");*/

  // If we're in the middle of a string-type escape sequence, few things get us
  // out of that (only \a 0o030 0o032 0o033; and those fall through) and other
  // processing cannot override it, so resolve that case first.
  if (term.esc & ESC_STR) {
    if (*u == '\a' || *u == 0x18 || *u == 0x1a || *u == 0x1b) {
      term.esc &= ~(ESC_START|ESC_STR); term.esc |= ESC_STR_END;
    } else {
      char *dest = strEsc.buf + strEsc.len; do {
        if (++strEsc.len >= STR_BUF_SIZ) {
          // The escape sequence is long, likely a bug in the application
          // generating it. My choice is just to drop and ignore what we have.
          fprintf(stderr, "Ignoring (start of) (probably buggy) long string-"
            "type escape sequence.\n"); strEsc.len = 0; term.esc = 0; return;
        }
        *dest++ = *u++;
      } while (*u); return;
  }  }
  // Any control codes (that weren't captured in a string-type escape sequence
  // above) must be performed: they can be embedded inside a control sequence.
  if (ISCONTROL(*u)) {tcontrolcode(*u); return;}
  // Handle any other escape sequences.
  if (term.esc & ESC_START) {
    if (term.esc & ESC_CSI) {
      csiEsc.buf[csiEsc.len++] = *u;
      if (BETWEEN(*u, 0x40, 0x7E) || csiEsc.len >= sizeof(csiEsc.buf) - 1) {
        term.esc = 0; csiparse(); csihandle();}
      return;
    } else if (term.esc & ESC_UTF8) ;
    else if (term.esc & ESC_ALTCHARSET) ;
    else if (term.esc & ESC_TEST) ;
    else if (!eschandle(*u)) return; // sequence already finished
    term.esc = 0; return;
  }
  if (selected(term.c.x, term.c.y)) selclear();

  // Ok, we're going to print this non-control & non-escape codepoint.
  // First see if it grapheme-composes with the previous column.
  // I think most composes will be when the previous cell is width=2.
  if (unlikely(term.c.x &&
      !(term.line[term.c.y][term.c.x - 1].mode & ATTR_WDUMMY) &&
      tryCompose(term.line[term.c.y][term.c.x - 1].u, u))) return;
  if (unlikely(term.c.x > 1 &&
      (term.line[term.c.y][term.c.x - 1].mode & ATTR_WDUMMY) &&
      tryCompose(term.line[term.c.y][term.c.x - 2].u, u))) return;
  // Since it didn't compose, it goes in the current cell.
  //int len = 1, width = 1;
  int width = 1;
  if (unlikely(u[1])) {
    int len = strlen(u);
    width = (1 == u8_width((uchar*)u, len, "")) ? 1 : 2;
    //width = 1;
  }
  Glyph *g = &term.line[term.c.y][term.c.x];
  if (T_IS_SET(MODE_WRAP) && (term.c.state & CURSOR_WRAPNEXT)) {
    g->mode |= ATTR_WRAP;
    tnewline(1);
    g = &term.line[term.c.y][term.c.x];
  }
  // insert mode means we have to pushToTheRight subsequent cells
  if (T_IS_SET(MODE_INSERT) && term.c.x + width < term.col) {
    memmove(g + width, g, (term.col - term.c.x - width) * sizeof(Glyph));
    g->mode &= ~ATTR_WIDE;
  }

  if (term.c.x + width > term.col) {
    if (T_IS_SET(MODE_WRAP)) tnewline(1);
    else tmoveto(term.col - width, term.c.y);
    g = &term.line[term.c.y][term.c.x];
  }
  tsetchar(u, &term.c.attr, term.c.x, term.c.y);
  if (width == 2) {
    g->mode |= ATTR_WIDE;
    if (term.c.x + 1 < term.col) {
      if (g[1].mode == ATTR_WIDE && term.c.x + 2 < term.col) {
        *g[2].u = ' '; g[2].u[1] = 0; g[2].mode &= ~ATTR_WDUMMY;}
      *g[1].u = 0; g[1].mode = ATTR_WDUMMY;
    }
  }
  if (term.c.x + width < term.col) tmoveto(term.c.x + width, term.c.y);
  else term.c.state |= CURSOR_WRAPNEXT;
}
void
ttyresize(int tw, int th) {
  struct winsize w;
  w.ws_row = term.row;
  w.ws_col = term.col;
  w.ws_xpixel = tw;
  w.ws_ypixel = th;
  if (ioctl(cmdfd, TIOCSWINSZ, &w) < 0)
    fprintf(stderr, "Couldn't set window size: %s\n", strerror(errno));
}
void
ttyhangup(void) {
  kill(pid, SIGHUP); // Send SIGHUP to shell
}
int
tattrset(int attr) {
  for (int i = 0; i < term.row - 1; i++) for (int j = 0; j < term.col-1; j++)
    if (term.line[i][j].mode & attr) return 1;
  return 0;
}
void
tsetdirtattr(int attr) {
  for (int i = 0; i < term.row - 1; i++) for (int j = 0; j < term.col-1; j++)
    if (term.line[i][j].mode & attr) {tsetdirt(i, i); break;}
}
void
tresize(int col, int row) {
  int i, minrow = MIN(row, term.row), mincol = MIN(col, term.col);
  TCursor c;

  if (col < 1 || row < 1) {
    fprintf(stderr, "tresize: error resizing to %dx%d\n", col, row);
    return;
  }

  // slide screen to keep cursor where we expect it. tscrollup would work here,
  // but we can optimize to memmove because we're freeing the earlier lines
  for (i = 0; i <= term.c.y - row; i++) {free(term.line[i]); free(term.alt[i]);
  }
  // ensure that both src and dst are not NULL
  if (i > 0) {
    memmove(term.line, term.line + i, row * sizeof(Line));
    memmove(term.alt, term.alt + i, row * sizeof(Line));
  }
  for (i += row; i < term.row; i++) {free(term.line[i]); free(term.alt[i]);}

  // resize to new height
  term.line = xrealloc(term.line, row * sizeof(Line));
  term.alt = xrealloc(term.alt, row * sizeof(Line));
  term.dirty = xrealloc(term.dirty, row * sizeof(*term.dirty));

  for (i = 0; i < minrow; i++) { // resize each row to new width, zero-padding
    term.line[i] = xrealloc(term.line[i], col * sizeof(Glyph));
    term.alt[i] = xrealloc(term.alt[i], col * sizeof(Glyph));
  }

  for (/* i = minrow */; i < row; i++) { // allocate any new rows
    term.line[i] = xmalloc(col * sizeof(Glyph));
    term.alt[i] = xmalloc(col * sizeof(Glyph));
  }
  // update terminal size & reset scrolling region
  term.col = col; term.row = row; tsetscroll(0, row - 1);
  tmoveto(term.c.x, term.c.y); // make use of the LIMIT in tmoveto
  c = term.c;
  for (i = 0; i < 2; i++) { // Clearing both screens (it makes dirty all lines)
    if (mincol < col && 0 < minrow)
      tclearregion(mincol, 0, col - 1, minrow - 1);
    if (0 < col && minrow < row) tclearregion(0, minrow, col - 1, row - 1);
    tswapscreen();
    tcursor(CURSOR_LOAD);
  }
  term.c = c;
}
void
tnew(int col, int row) {
  term = (Term){.c = {.attr = {.fg = fgPalI, .bg = bgPalI}}};
  tresize(col, row); treset();
}

// XEMBED messages
#define XEMBED_FOCUS_IN  4
#define XEMBED_FOCUS_OUT 5

void
clippaste(const Arg *dummy) {
  xcb_convert_selection(xw.c, xw.win, xw.atom.clipboard, xsel.xtarget,
    xw.atom.clipboard, XCB_CURRENT_TIME); 
}
void
selpaste(const Arg *dummy) {
  //debug("selpaste()\n");
  xcb_convert_selection(xw.c, xw.win, XCB_ATOM_PRIMARY, xsel.xtarget,
    XCB_ATOM_PRIMARY, XCB_CURRENT_TIME);
  //xcb_flush(xw.c); // seems unneeded?
}
void
numlock(const Arg *dummy) {
  win.mode ^= MODE_NUMLOCK;
}
void
xresize(int col, int row) {
  win.tw = col * win.cw; win.th = row * win.ch; cairo_destroy(xw.cairo);
  cairo_surface_destroy(xw.cairoSurf);
  //XFreePixmap(xw.dpy, xw.pbuf);
  //xw.pbuf = XCreatePixmap(xw.dpy, xw.win, win.w, win.h, DefaultDepth(xw.dpy,
  //  xw.scr));
  //xclear();
  xw.cairoSurf = cairo_xcb_surface_create(xw.c, xw.win, xw.vis, win.w, win.h);
  xw.cairo = cairo_create(xw.cairoSurf);
  xw.layout = pango_cairo_create_layout(xw.cairo);
  xw.fontD = pango_font_description_from_string("Liberation Mono 10.5");
  xw.isBold = 0; xw.isItalic = 0;
}
void
cresize(int width, int height) {
  if (width != 0) win.w = width;
  if (height != 0) win.h = height;
  int col = MAX(1, (win.w - 2 * borderpx) / win.cw),
      row = MAX(1, (win.h - 2 * borderpx) / win.ch);
  tresize(col, row);
  xresize(col, row);
  ttyresize(win.tw, win.th);
}
int
xgeommasktogravity(int mask) {
  switch (mask & (XNegative|YNegative)) {
  case 0:
    return NorthWestGravity;
  case XNegative:
    return NorthEastGravity;
  case YNegative:
    return SouthWestGravity;
  }

  return SouthEastGravity;
}
/*void
xhints(void) {
  XWMHints wm = {.flags = InputHint, .input = 1};
  XSizeHints *sizeh = XAllocSizeHints();
  sizeh->flags = PSize | PResizeInc | PBaseSize | PMinSize;
  sizeh->height = win.h;
  sizeh->width = win.w;
  sizeh->height_inc = win.ch;
  sizeh->width_inc = win.cw;
  sizeh->base_height = 2 * borderpx;
  sizeh->base_width = 2 * borderpx;
  sizeh->min_height = win.ch + 2 * borderpx;
  sizeh->min_width = win.cw + 2 * borderpx;
  if (xw.isfixed) {
    sizeh->flags |= PMaxSize;
    sizeh->min_width = sizeh->max_width = win.w;
    sizeh->min_height = sizeh->max_height = win.h;
  }
  if (xw.gm & (XValue|YValue)) {
    sizeh->flags |= USPosition | PWinGravity;
    sizeh->x = xw.l;
    sizeh->y = xw.t;
    sizeh->win_gravity = xgeommasktogravity(xw.gm);
  }
  XSetWMProperties(xw.dpy, xw.win, NULL, NULL, NULL, 0, sizeh, &wm, NULL);
  XFree(sizeh);
}*/

void
ttysend(const Arg *arg) {
  tellShell(arg->s, strlen(arg->s), 1);
}
int
evcol(xcb_generic_event_t *e) {
  int x = ((xcb_button_press_event_t*)e)->event_x - borderpx;
  LIMIT(x, 0, win.tw - 1); return x / win.cw;
}
int
evrow(xcb_generic_event_t *e) {
  int y = ((xcb_button_press_event_t*)e)->event_y - borderpx;
  LIMIT(y, 0, win.th - 1); return y / win.ch;
}
int
match(uint mask, uint state) {
  return mask == XK_ANY_MOD || mask == (state & ~ignoremod);
}
void
mousesel(xcb_generic_event_t *e, int done) {
  // This cast is safe for button press, release, and motion events.
  xcb_button_press_event_t *ev = (xcb_button_press_event_t*)e;
  int seltype = SEL_REGULAR;
  uint state = ev->state & ~(XCB_BUTTON_MASK_1 | forcemousemod);
  for (int type = 1; type < LEN(selmasks); ++type) {
    if (match(selmasks[type], state)) {seltype = type; break;}
  }
  selextend(evcol(e), evrow(e), seltype, done);
  if (done) setsel(getsel(), ev->time);
}
void
mousereport(xcb_generic_event_t *e) {
  // Cast the generic event to a mouse event.
  xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
  int len, btn, code, x = evcol(e), y = evrow(e), state = ev->state;
  static int ox, oy;
  char buf[40];

  if (e->response_type == XCB_MOTION_NOTIFY) {
    if (x == ox && y == oy) return;
    if (!IS_SET(MODE_MOUSEMOTION) && !IS_SET(MODE_MOUSEMANY)) return;
    // MODE_MOUSEMOTION: no reporting if no button is pressed
    if (IS_SET(MODE_MOUSEMOTION) && buttons == 0) return;
    // Set btn to lowest-numbered pressed button, or 12 if no buttons are
    // pressed.
    for (btn = 1; btn <= 11 && !(buttons & (1<<(btn-1))); btn++) ;
    code = 32;
  } else {
    // The button number is in the 'detail' field for XCB events.
    btn = ev->detail;
    // Only buttons 1 through 11 can be encoded
    if (btn < 1 || btn > 11) return;
    if (e->response_type == XCB_BUTTON_RELEASE) {
      // MODE_MOUSEX10: no button release reporting
      if (IS_SET(MODE_MOUSEX10)) return;
      // Don't send release events for the scroll wheel
      if (btn == 4 || btn == 5) return;
    }
    code = 0;
  }

  ox = x; oy = y;

  // Encode btn into code. If no button is pressed for a motion event in
  // MODE_MOUSEMANY, then encode it as a release.
  if ((!IS_SET(MODE_MOUSESGR) && e->response_type == XCB_BUTTON_RELEASE) || btn == 12)
    code += 3;
  else if (btn >= 8) code += 128 + btn - 8;
  else if (btn >= 4) code += 64 + btn - 4;
  else code += btn - 1;

  if (!IS_SET(MODE_MOUSEX10)) code +=
    ((state & XCB_KEY_BUT_MASK_SHIFT)   ?  4 : 0) +
    ((state & XCB_KEY_BUT_MASK_MOD_1)   ?  8 : 0) + // meta key: alt
    ((state & XCB_KEY_BUT_MASK_CONTROL) ? 16 : 0);

  if (IS_SET(MODE_MOUSESGR))
    len = snprintf(buf, sizeof(buf), "\x1b[<%d;%d;%d%c", code, x+1, y+1,
      e->response_type == XCB_BUTTON_RELEASE ? 'm' : 'M');
  else if (x < 223 && y < 223)
    len = snprintf(buf, sizeof(buf), "\x1b[M%c%c%c", 32+code, 32+x+1, 32+y+1);
  else return;

  tellShell(buf, len, 0);
}
uint
buttonmask(uint button) {
  return button == Button1 ? Button1Mask : button == Button2 ? Button2Mask
       : button == Button3 ? Button3Mask : button == Button4 ? Button4Mask
       : button == Button5 ? Button5Mask : 0;
}
typedef struct {uint mod; uint button; void (*func)(const Arg *);
  const Arg arg; uint release;} MouseShortcut;
void
selnotify(xcb_generic_event_t *e) {
  xcb_selection_notify_event_t *snE = (xcb_selection_notify_event_t *)e;
  if (snE->property == XCB_NONE) {
    fprintf(stderr, "selnotify: Conversion failed\n"); return;}
  xcb_get_property_cookie_t cookie = xcb_get_property(xw.c, 0, xw.win,
    snE->property, snE->target, 0, ~0U);
  xcb_get_property_reply_t *reply = xcb_get_property_reply(xw.c, cookie, NULL);
  if (!reply) {
    fprintf(stderr, "selnotify: xcb_get_property_reply failed\n"); return;}
  if (reply->type == snE->target && reply->format == 8) tellShell(
    xcb_get_property_value(reply), xcb_get_property_value_length(reply), 0);
  else fprintf(stderr, "selnotify: Received unsupported format "
    "(type=%u, format=%i)\n", reply->type, reply->format);
  free(reply);
  xcb_delete_property(xw.c, xw.win, snE->property);
  xcb_flush(xw.c);
}
void
propnotify(xcb_generic_event_t *ev) {
  xcb_property_notify_event_t *xpev = (xcb_property_notify_event_t*)ev;
  if (xpev->state == XCB_PROPERTY_NEW_VALUE && (xpev->atom == XCB_ATOM_PRIMARY
      || xpev->atom == xw.atom.clipboard)) selnotify(ev);
}
void
selclear_(xcb_generic_event_t *e) {
  selclear();
}
void
selrequest(xcb_generic_event_t *e) {
  xcb_selection_request_event_t *rE = (xcb_selection_request_event_t *)e;
  xcb_selection_notify_event_t nE;
  memset(&nE, 0, sizeof(xcb_selection_notify_event_t)); // FIXME: needed?
  nE.response_type = XCB_SELECTION_NOTIFY;
  nE.requestor = rE->requestor;
  nE.selection = rE->selection;
  nE.target = rE->target;
  nE.time = rE->time;
  nE.property = rE->property;
  if (nE.property == XCB_NONE) nE.property = rE->target; // FIXME: needed?
  if (rE->target == xw.atom.targets) { // say we offer only utf8
    xcb_atom_t supported_targets[] = {xsel.xtarget};
    xcb_change_property(xw.c, XCB_PROP_MODE_REPLACE, rE->requestor,
      nE.property, XCB_ATOM_ATOM, 32, 1, supported_targets);
    nE.property = rE->property; // Mark as successful
  } else if (rE->target == xsel.xtarget || rE->target == XCB_ATOM_STRING) {
    char *seltext;
    if (XCB_ATOM_PRIMARY == rE->selection) seltext = xsel.primary;
    else if (rE->selection == xw.atom.clipboard) seltext = xsel.clipboard;
    else {
      fprintf(stderr, "Unhandled clipboard selection 0x%x\n", rE->selection);
      return; // just return, sends default reject response
    }
    xcb_change_property(xw.c, XCB_PROP_MODE_REPLACE, rE->requestor,
      nE.property, rE->target, 8, strlen(seltext), seltext);
    nE.property = rE->property; // Mark as successful
  }
  xcb_send_event(xw.c, 0, rE->requestor, XCB_EVENT_MASK_NO_EVENT, (char*)&nE);
}
MouseShortcut mshortcuts[] = { // Overloading Button1 disables selection.
  // mask       button   function  argument           release
  { XK_ANY_MOD, Button2, selpaste, {.i = 0},          1 },
  { ShiftMask,  Button4, ttysend,  {.s = "\x1b[5;2~"}   },
  { XK_ANY_MOD, Button4, ttysend,  {.s = "\031"}        },
  { ShiftMask,  Button5, ttysend,  {.s = "\x1b[6;2~"}   },
  { XK_ANY_MOD, Button5, ttysend,  {.s = "\005"}        },
};

int
mouseaction(xcb_generic_event_t *e, uint release) {
  xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;

  // ignore Button<N>mask for Button<N> - it's set on release
  uint state = ev->state & ~buttonmask(ev->detail);

  MouseShortcut *ms;
  for (ms = mshortcuts; ms < mshortcuts + LEN(mshortcuts); ms++) {
    if (ms->release == release &&
        ms->button == ev->detail &&
        (match(ms->mod, state) ||  // exact or forced
         match(ms->mod, state & ~forcemousemod))) {
      ms->func(&(ms->arg));
      return 1;
    }
  }
  return 0;
}
void
bpress(xcb_generic_event_t *e) {
  xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
  int btn = ev->detail, snap;
  struct timespec now;

  if (1 <= btn && btn <= 11) buttons |= 1 << (btn-1);

  if (IS_SET(MODE_MOUSE) && !(ev->state & forcemousemod)) {
    mousereport(e);
    return;
  }

  if (mouseaction(e, 0)) return;

  if (btn == XCB_BUTTON_INDEX_1) {
    // If the user clicks below predefined timeouts specific snapping
    // behavior is exposed
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (TIMEDIFF(now, xsel.tclick2) <= tripleclicktimeout) snap = SNAP_LINE;
    else if (TIMEDIFF(now, xsel.tclick1) <= doubleclicktimeout)
      snap = SNAP_WORD;
    else snap = 0;
    xsel.tclick2 = xsel.tclick1;
    xsel.tclick1 = now;
    selstart(evcol(e), evrow(e), snap);
  }
}
// Mod1Mask is another possible mask
#define TERMMOD (ControlMask|ShiftMask)
typedef struct {uint mod; KeySym keysym; void (*func)(const Arg *);
  const Arg arg;} Shortcut;
Shortcut shortcuts[] = {
  // mask                 keysym          function        argument
  { TERMMOD,              XK_C,           clipcopy,       {.i =  0} },
  { TERMMOD,              XK_V,           clippaste,      {.i =  0} },
  { TERMMOD,              XK_Y,           selpaste,       {.i =  0} },
  { ShiftMask,            XK_Insert,      selpaste,       {.i =  0} },
  { TERMMOD,              XK_Num_Lock,    numlock,        {.i =  0} },
};
void
brelease(xcb_generic_event_t *e) {
  xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
  int btn = ev->detail;
  if (1 <= btn && btn <= 11) buttons &= ~(1 << (btn-1));
  if (IS_SET(MODE_MOUSE) && !(ev->state & forcemousemod)) {
    mousereport(e); return;}
  if (mouseaction(e, 1)) return;
  if (btn == XCB_BUTTON_INDEX_1) mousesel(e, 1);
}
void
handleMouseNotify(xcb_generic_event_t *e) {
  xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
  if (IS_SET(MODE_MOUSE) && !(ev->state & forcemousemod)) {
    mousereport(e); return;}
  mousesel(e, 0);
}
/*int
xicdestroy(XIC xim, XPointer client, XPointer call) {
  xw.ime.xic = NU; return 1;
}
int ximopen(Display *dpy); // -> ximdestroy -> ximinstantiate -> ximopen

void
ximinstantiate(Display *dpy, XPointer client, XPointer call) {
  if (ximopen(dpy)) XUnregisterIMInstantiateCallback(xw.dpy, NULL, NULL, NULL,
    ximinstantiate, NULL);
}
void
ximdestroy(XIM xim, XPointer client, XPointer call) {
  xw.ime.xim = NULL;
  XRegisterIMInstantiateCallback(xw.dpy, NULL, NULL, NULL, ximinstantiate,
    NULL);
  XFree(xw.ime.spotlist);
}
int
ximopen(Display *dpy) {
  XIMCallback imdestroy = {.client_data = NULL, .callback = ximdestroy};
  XICCallback icdestroy = {.client_data = NULL, .callback = xicdestroy};
  xw.ime.xim = XOpenIM(xw.dpy, NULL, NULL, NULL);
  if (xw.ime.xim == NULL) {fprintf(stderr, "XOpenIM failed\n"); return 0;}
  if (XSetIMValues(xw.ime.xim, XNDestroyCallback, &imdestroy, NULL))
    fprintf(stderr, "XSetIMValues: Could not set XNDestroyCallback.\n");
  xw.ime.spotlist = XVaCreateNestedList(0, XNSpotLocation, &xw.ime.spot, NULL);
  if (!xw.ime.xic) xw.ime.xic = XCreateIC(xw.ime.xim, XNInputStyle,
    XIMPreeditNothing | XIMStatusNothing, XNClientWindow, xw.win,
    XNFocusWindow, xw.win, XNDestroyCallback, &icdestroy, NULL);
    // added XNFocusWindow, xw.win trying to get XIM to work
  if (!xw.ime.xic) fprintf(stderr, "XCreateIC failed\n");
  return 1;
}*/
void
xsetenv(void) {
  char buf[sizeof(long) * 8 + 1]; snprintf(buf, sizeof(buf), "%u", xw.win);
  setenv("WINDOWID", buf, 1);
}
void
expose(xcb_generic_event_t *ev) {
  redraw();
}
void
unmap(xcb_generic_event_t *ev) {
  win.mode &= ~MODE_VISIBLE;
}
void
visibilityNotify(xcb_generic_event_t *ev) {
  xcb_visibility_notify_event_t *e = (xcb_visibility_notify_event_t *)ev;
  MODBIT(win.mode, e->state != XCB_VISIBILITY_FULLY_OBSCURED, MODE_VISIBLE);
}
void
focus(xcb_generic_event_t *ev) {
  xcb_focus_in_event_t *e = (xcb_focus_in_event_t *)ev;
  if (e->mode == XCB_NOTIFY_MODE_GRAB) return;
  if (ev->response_type == XCB_FOCUS_IN) {
    if (xw.ime.xic) {
      xcb_xim_set_ic_focus(xw.ime.xim, xw.ime.xic);
      //debug("XIC focused\n");
    }
    win.mode |= MODE_FOCUSED;
    //xseturgency(0);
    if (IS_SET(MODE_FOCUS)) tellShell("\x1b[I", 3, 0);
  } else {
    if (xw.ime.xic) {
      xcb_xim_unset_ic_focus(xw.ime.xim, xw.ime.xic);
      //debug("XIC unfocused\n");
    }
    win.mode &= ~MODE_FOCUSED;
    if (IS_SET(MODE_FOCUS)) tellShell("\x1b[O", 3, 0);
  }
}
char*
ksym2strIfFunnyInTerms(KeySym k, uint state) {
  for (Key *kp = ksymsFunnyInTerms;
      kp < ksymsFunnyInTerms + LEN(ksymsFunnyInTerms); kp++) {
    if (kp->k != k || !match(kp->mask, state)) continue;
    if (IS_SET(MODE_APPKEYPAD) ? kp->appkey < 0 : kp->appkey > 0) continue;
    if (IS_SET(MODE_NUMLOCK) && kp->appkey == 2) continue;
    if (IS_SET(MODE_APPCURSOR) ? kp->appcursor < 0 : kp->appcursor > 0)
      continue;
    return kp->s;
  }
  return NULL;
}
void
cmessage(xcb_generic_event_t *ev) {
  xcb_client_message_event_t *e = (xcb_client_message_event_t *)ev;
  if (e->type == xw.atom.xembed && e->format == 32) {
    if (e->data.data32[1] == XEMBED_FOCUS_IN) win.mode |= MODE_FOCUSED;
    else if (e->data.data32[1] == XEMBED_FOCUS_OUT) win.mode &= ~MODE_FOCUSED;
  } else if (e->data.data32[0] == xw.atom.wmDeleteWindow)
    {ttyhangup(); exit(0);}
}
void
resize(xcb_generic_event_t *e) {
  xcb_configure_notify_event_t *ev = (xcb_configure_notify_event_t *)e;
  if (ev->width == win.w && ev->height == win.h) return;
  cresize(ev->width, ev->height);
}
void
usage(void) {
  die("usage: %s [-ai] [-b backgroundColorIndex] [-g geometry] [-t title]\n"
  "  [[-e] command [args ...]]\n"
  "%s [-ai] [-b backgroundColorIndex] [-g geometry]  [-t title]\n"
  "  -l line [stty_args ...]\n",
  argv0, argv0);
}
void
kpress(xcb_generic_event_t *ev) {
  xcb_key_press_event_t *e = (xcb_key_press_event_t *)ev;
  xcb_keysym_t ksym;
  Shortcut *bp;
  char buf[64];
  KeySym xlib_keysym;
  int len;
  if (IS_SET(MODE_KBDLOCK)) return;
  ksym = xcb_key_symbols_get_keysym(xw.keysyms, e->detail, 0);
  char *str = ksym2strIfFunnyInTerms(ksym, e->state);
  if (str) {tellShell(str, strlen(str), 1); return;}
  for (bp = shortcuts; bp < shortcuts + LEN(shortcuts); bp++)
    if (ksym == bp->keysym && match(bp->mod, e->state))
      {bp->func(&(bp->arg)); return;}
  XKeyEvent xlib_ev;
  xlib_ev.type = KeyPress;
  xlib_ev.display = xw.dpy;
  xlib_ev.window = (Window)e->event;
  xlib_ev.root = DefaultRootWindow(xw.dpy);
  xlib_ev.subwindow = (Window)e->child;
  xlib_ev.time = e->time;
  xlib_ev.x = e->event_x;
  xlib_ev.y = e->event_y;
  xlib_ev.x_root = e->root_x;
  xlib_ev.y_root = e->root_y;
  xlib_ev.state = e->state;
  xlib_ev.keycode = e->detail;
  xlib_ev.same_screen = True;
  /*if (xw.ime.xic) {
    Status status;
    len = XmbLookupString(xw.ime.xic, &xlib_ev, buf, sizeof(buf) - 1,
      &xlib_keysym, &status);
    if (status == XBufferOverflow) return;
  } else*/ len = XLookupString(&xlib_ev, buf, sizeof(buf) - 1, &xlib_keysym, 0);
  if (!len) return; // some keys don't produce text like just pressing Shift
  buf[len] = '\0'; tellShell(buf, len, 1);
  //u1t evTyp = ev->response_type & 0x7f;
  //debug("%c %i %.*s\n", XCB_KEY_PRESS == evTyp ? 'd' : 'u', len, len, buf);
}
// 34 because XCB_CLIENT_MESSAGE = 33 highest even w/ XCB_SELECTION_CLEAR
void (*handler[34])(xcb_generic_event_t*) = {
  //[XCB_KEY_PRESS] = kpress,
  [XCB_CLIENT_MESSAGE] = cmessage,
  [XCB_CONFIGURE_NOTIFY] = resize,
  [XCB_VISIBILITY_NOTIFY] = visibilityNotify,
  [XCB_UNMAP_NOTIFY] = unmap,
  [XCB_EXPOSE] = expose,
  [XCB_FOCUS_IN] = focus,
  [XCB_FOCUS_OUT] = focus,
  [XCB_MOTION_NOTIFY] = handleMouseNotify,
  [XCB_BUTTON_PRESS] = bpress,
  [XCB_BUTTON_RELEASE] = brelease,
  // If you want the selection to disappear when you select something different
  // in another window, add: [XCB_SELECTION_CLEAR] = selclear_,
  [XCB_SELECTION_NOTIFY] = selnotify,
  // PropertyNotify is only turned on when there is some INCR transfer
  // happening for the selection retrieval
  [XCB_PROPERTY_NOTIFY] = propnotify,
  [XCB_SELECTION_REQUEST] = selrequest};
/*void
ximDebugLogger(const char *fmt, ...) {
  va_list a; va_start(a, fmt); vprintf(fmt, a); va_end(a);
}*/
void
ximForwardEvCb(xcb_xim_t*, xcb_xic_t, xcb_key_press_event_t *e, void*) {
  //debug("ximForwardEvCb calling kpress\n");
  kpress((xcb_generic_event_t*)e);
}
void
ximCommitStrCb(xcb_xim_t *xim, xcb_xic_t xic, uint32_t flag, char *str,
    uint32_t len, uint32_t *keysym, size_t nKeySym, void *user_data) {
  if (xcb_xim_get_encoding(xim) == XCB_XIM_UTF8_STRING) {
    //debug("key commit utf8: %.*s\n", len, str);
    tellShell(str, len, 1);
  } else if (xcb_xim_get_encoding(xim) == XCB_XIM_COMPOUND_TEXT) {
    size_t utf8len = 0;
    char *utf8 = xcb_compound_text_to_utf8(str, len, &utf8len);
    //if (!utf8) return;
    //debug("key commit: %zu %.*s\n", utf8len, utf8len, utf8);
    tellShell(utf8, utf8len, 1);
  }
}
void
ximDisconCb(xcb_xim_t *xim, void *userData) {
  debug("Disconnected from input method server.\n"); xw.ime.xic = 0;
}
xcb_xim_im_callback callback = {
  .forward_event = ximForwardEvCb,
  .commit_string = ximCommitStrCb,
  .disconnected = ximDisconCb};
void
xicCreateCb(xcb_xim_t *xim, xcb_xic_t newXic, void *userData) {
  xw.ime.xic = newXic;
  if (xw.ime.xic) {
    //debug("xic:%u\n", xw.ime.xic);
    xcb_xim_set_ic_focus(xim, xw.ime.xic);
  }
}
void
ximOpenCb(xcb_xim_t *xim, void *user_data) {
  uint32_t input_style = XCB_IM_PreeditPosition | XCB_IM_StatusArea;
  xcb_point_t spot; spot.x = 0; spot.y = 0;
  xcb_xim_nested_list nested =
    xcb_xim_create_nested_list(xim, XCB_XIM_XNSpotLocation, &spot, NULL);
  xcb_xim_create_ic(xim, xicCreateCb, NULL, XCB_XIM_XNInputStyle,
    &input_style, XCB_XIM_XNClientWindow, &xw.win, XCB_XIM_XNFocusWindow,
    &xw.win, XCB_XIM_XNPreeditAttributes, &nested, NULL);
  free(nested.data);
}
void
xinit(int cols, int rows) {
  xcb_compound_text_init(); // For me fcitx5 likes compound, not direct utf8.
  xcb_cursor_t cursor;
  //pid_t thispid = getpid();
  int scrDefN; xw.c = eoz(xcb_connect(NULL, &scrDefN));
  xcb_screen_t *screen = xcb_aux_get_screen(xw.c, scrDefN); eoz(screen);
  const xcb_setup_t *setup = xcb_get_setup(xw.c);
  xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);
  xw.scr = iter.data; // Get the first screen.
  for (xcb_depth_iterator_t depthIt = xcb_screen_allowed_depths_iterator(
      xw.scr); depthIt.rem; xcb_depth_next(&depthIt))
    for (xcb_visualtype_iterator_t visIt = xcb_depth_visuals_iterator(
        depthIt.data); visIt.rem; xcb_visualtype_next(&visIt))
      if (xw.scr->root_visual == visIt.data->visual_id) {
        xw.vis = visIt.data; goto foundVis;
      }
  eoz(0);
  foundVis:

  win.cw = 8; win.ch = 16;
  win.w = 2 * borderpx + cols * win.cw;
  win.h = 2 * borderpx + rows * win.ch;
  xw.win = xcb_generate_id(xw.c);
  xw.evMask = XCB_EVENT_MASK_FOCUS_CHANGE | XCB_EVENT_MASK_KEY_PRESS |
    XCB_EVENT_MASK_KEY_RELEASE | XCB_EVENT_MASK_EXPOSURE |
    XCB_EVENT_MASK_VISIBILITY_CHANGE | XCB_EVENT_MASK_STRUCTURE_NOTIFY |
    XCB_EVENT_MASK_BUTTON_MOTION | XCB_EVENT_MASK_BUTTON_PRESS |
    XCB_EVENT_MASK_BUTTON_RELEASE;
  xcb_create_window(xw.c, xw.scr->root_depth, xw.win, xw.scr->root, 0, 0,
    win.w, win.h, 0, XCB_WINDOW_CLASS_INPUT_OUTPUT, xw.vis->visual_id,
    XCB_CW_EVENT_MASK, (uint32_t[]){xw.evMask});

  xw.ime.xim = xcb_xim_create(xw.c, scrDefN, NULL);
  xcb_xim_set_im_callback(xw.ime.xim, &callback, NULL);
  //xcb_xim_set_log_handler(xw.ime.xim, ximDebugLogger);
  xcb_xim_set_use_compound_text(xw.ime.xim, true);
  xcb_xim_set_use_utf8_string(xw.ime.xim, true);
  xcb_xim_open(xw.ime.xim, ximOpenCb, true, NULL);
  
  xw.dpy = eoz(XOpenDisplay(NULL));
  xw.keysyms = eoz(xcb_key_symbols_alloc(xw.c));

  const char *atomNames[] = {"CLIPBOARD", "TARGETS", "TEXT", "UTF8_STRING",
    "WM_DELETE_WINDOW", "_NET_WM_ICON_NAME", "_NET_WM_NAME", "WM_PROTOCOLS",
    "_XEMBED"};
  const int atom_count = sizeof(atomNames) / sizeof(atomNames[0]);
  xcb_intern_atom_cookie_t atom_cookies[atom_count];
  xcb_atom_t atom_ptrs[atom_count];

  for (int i = 0; i < atom_count; i++) atom_cookies[i] = xcb_intern_atom(xw.c,
    0, strlen(atomNames[i]), atomNames[i]);
  for (int i = 0; i < atom_count; i++) {
    xcb_intern_atom_reply_t *reply =
      xcb_intern_atom_reply(xw.c, atom_cookies[i], NULL);
    if (reply) {atom_ptrs[i] = reply->atom; free(reply);}
    else atom_ptrs[i] = XCB_NONE;
  }
  xw.atom.clipboard = atom_ptrs[0]; xw.atom.targets = atom_ptrs[1];
  xw.atom.text = atom_ptrs[2]; xw.atom.utf8string = atom_ptrs[3];
  xw.atom.wmDeleteWindow = atom_ptrs[4]; xw.atom.wmIconName = atom_ptrs[5];
  xw.atom.wmName = atom_ptrs[6]; xw.atom.wmProtocols = atom_ptrs[7];
  xw.atom.xembed = atom_ptrs[8];
  xcb_gcontext_t gc = xcb_generate_id(xw.c);
  xcb_create_gc(xw.c, gc, xw.win, XCB_GC_GRAPHICS_EXPOSURES, (uint32_t[]){0});
    // 0 means don't generate GraphicsExpose events

  xw.cairoSurf = cairo_xcb_surface_create(xw.c, xw.win, xw.vis, win.w, win.h);
  xw.cairo = cairo_create(xw.cairoSurf);

  xcb_cursor_context_t *ctx;
  if (xcb_cursor_context_new(xw.c, xw.scr, &ctx) >= 0) {
    cursor = xcb_cursor_load_cursor(ctx, "xterm");
    xcb_change_window_attributes(xw.c, xw.win, XCB_CW_CURSOR, &cursor);
    xcb_free_cursor(xw.c, cursor);
    xcb_cursor_context_free(ctx);
  }

  xcb_change_property(xw.c, XCB_PROP_MODE_REPLACE, xw.win,
    xw.atom.wmProtocols, XCB_ATOM_ATOM, 32, 1, &xw.atom.wmDeleteWindow);
  win.mode = MODE_NUMLOCK;
  xcb_map_window(xw.c, xw.win); xcb_flush(xw.c);

  clock_gettime(CLOCK_MONOTONIC, &xsel.tclick1);
  clock_gettime(CLOCK_MONOTONIC, &xsel.tclick2);
  xsel.primary = NULL; xsel.clipboard = NULL;
  xsel.xtarget = xw.atom.utf8string;
}
int
main(int argc, char *argv[]) {
  win.cursor = 2;
  int i = 16; for (int r = 0; r < 6; r++) for (int g = 0; g < 6; g++) 
    for (int b = 0; b < 6; b++) {
      palette[i  ].r = r ? (r * 40 + 55) : 0;
      palette[i  ].g = g ? (g * 40 + 55) : 0;
      palette[i++].b = b ? (b * 40 + 55) : 0;
    }
  for (i = 0; i < 24; i++) {
    uint8_t gray = 8 + i * 10;
    palette[232 + i].r = gray;
    palette[232 + i].g = gray;
    palette[232 + i].b = gray;
  }
  palette[258] = (Rgb){255,253,255};
  palette[259] = (Rgb){255,255,247};
  xw.l = xw.t = 0; xw.isfixed = False; ARGBEGIN {
  case 'a': allowaltscreen = 0; break;
  case 'b': bgPalI = strtoul(EARGF(usage()), NULL, 10); break;
  case 'e': if (argc > 0) argc--, argv++; goto run;
  case 'g': xw.gm = XParseGeometry(EARGF(usage()), &xw.l, &xw.t, &cols, &rows);
    break;
  case 'i': xw.isfixed = 1; break;
  case 'l': opt_line = EARGF(usage()); break;
  case 't': opt_title = EARGF(usage()); break;
  default: usage();
  } ARGEND;
run:
  if (argc > 0) opt_cmd = argv; // eat all remaining arguments
  if (!opt_title) opt_title = (opt_line || !opt_cmd) ? "st" : opt_cmd[0];
  setlocale(LC_CTYPE, ""); XSetLocaleModifiers("");
  cols = MAX(cols, 1); rows = MAX(rows, 1); tnew(cols, rows);
  xinit(cols, rows); xsetenv(); selinit();

  xcb_generic_event_t *ev; int w = win.w, h = win.h; fd_set rfd;
  int xfd = xcb_get_file_descriptor(xw.c), ttyfd, xEv, drawing;
  struct timespec seltv, *tv, now, lastblink, trigger;
  double timeout;
  
  int gotMapNotify = 0;
  do {
    ev = xcb_wait_for_event(xw.c); eoz(ev);
    u1t evTyp = ev->response_type & 0x7f;
    if (xw.ime.xim) xcb_xim_filter_event(xw.ime.xim, ev);
    if (XCB_CONFIGURE_NOTIFY == evTyp) {
      xcb_configure_notify_event_t *cnEv = (xcb_configure_notify_event_t *)ev;
      w = cnEv->width; h = cnEv->height;
    }
    if (evTyp == XCB_MAP_NOTIFY) gotMapNotify = 1;
    free(ev);
  } while (!(gotMapNotify && xw.ime.xic));
  ttyfd = ttynew(opt_line, shell, opt_cmd);
  cresize(w, h);
  for (timeout = -1, drawing = 0, lastblink = (struct timespec){0};;) {
    FD_ZERO(&rfd); FD_SET(ttyfd, &rfd); FD_SET(xfd, &rfd);
    if (xcb_poll_for_queued_event(xw.c)) timeout = 0;
      // existing events might not set xfd
    seltv.tv_sec = timeout / 1E3;
    seltv.tv_nsec = 1E6 * (timeout - 1E3 * seltv.tv_sec);
    tv = timeout >= 0 ? &seltv : NULL;
    if (pselect(MAX(xfd, ttyfd) + 1, &rfd, NULL, NULL, tv, NULL) < 0) {
      if (errno == EINTR) continue;
      die("select failed: %s\n", strerror(errno));
    }
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (FD_ISSET(ttyfd, &rfd)) hearShell();
    xEv = 0;
    while ((ev = xcb_poll_for_event(xw.c))) {
      xEv = 1;
      //printXev(ev, stderr);
      u1t evTyp = ev->response_type & 0x7f;
      if (xcb_xim_filter_event(xw.ime.xim, ev) || (xw.ime.xic && XCB_KEY_PRESS
          == evTyp && xcb_xim_forward_event(xw.ime.xim, xw.ime.xic,
          (xcb_key_press_event_t*)ev))) {free(ev); continue;}
      if (handler[evTyp]) (handler[evTyp])(ev);
      free(ev);
    }
    // To reduce flicker and tearing, when new content or event triggers
    // drawing, we first wait a bit to ensure we got everything, and if nothing
    // new arrives: we draw. We start with trying to wait minlatency ms. If
    // more content arrives sooner, we retry with shorter and shorter periods,
    // and eventually draw even without idle after maxlatency ms. Typically
    // this results in low latency while interacting, maximum latency intervals
    // during `cat huge.txt`, and perfect sync with periodic updates from
    // animations/key-repeats/etc.
    if (xEv || FD_ISSET(ttyfd, &rfd)) {
      if (!drawing) {trigger = now; drawing = 1;}
      timeout = (maxlatency - TIMEDIFF(now,trigger)) / maxlatency * minlatency;
      if (timeout > 0) continue; // we have time, try to find idle
    }
    timeout = -1; // idle detected or maxlatency exhausted -> draw
    if (blinktimeout && tattrset(ATTR_BLINK)) {
      timeout = blinktimeout - TIMEDIFF(now, lastblink);
      if (timeout <= 0) {
        if (-timeout > blinktimeout) win.mode |= MODE_BLINK; // start visible
        win.mode ^= MODE_BLINK; tsetdirtattr(ATTR_BLINK);
        lastblink = now; timeout = blinktimeout;
      }
    }
    draw(); xcb_flush(xw.c); drawing = 0;
  }
}
