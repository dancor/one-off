#include <fluidsynth.h>
#include <stdlib.h>
#include <unistd.h>
#include "err.h"
#include <X11/Xlib.h>
int e2n(XEvent e){
  int k = e.xkey.keycode;
  if (k >= 52 && k <= 62) return k - 52 + 36; // z C2
  if (k == 111) return 47; // up B2
  if (k >= 38 && k <= 48) return k - 38 + 48; // a C3
  if (k == 36) return 59; // return B3
  if (k >= 24 && k <= 35) return k - 24 + 60; // q C4
  if (k >= 10 && k <= 21) return k - 10 + 72; // 1 C5
  if (k >= 67 && k <= 76) return k - 67 + 84; // f1 C6
  if (k == 95) return 94; // f11 A#6
  if (k == 96) return 95; // f12 B6
  if (k == 127) return 96; // pause C7
  if (k == 118) return 97; // insert C#7
  if (k == 119) return 98; // delete D7
  if (k == 110) return 99; // home D#7
  if (k == 112) return 100; // pgup E7
  if (k == 117) return 101; // pgdn F7
  if (k == 115) return 102; // end F#7
  if (k == 114) return 103; // right G7
  if (k == 116) return 104; // down G#7
  if (k == 113) return 105; // left A7
  if (k == 105) return 106; // ctrlR A#7
  if (k == 135) return 107; // winR B7
  if (k == 108) return 108; // altR C7
  return 0;
}
int main(int argc, char **argv){
  Display *d = XOpenDisplay(NULL);
  int n, scr = DefaultScreen(d);
  Window w = XCreateSimpleWindow(d, RootWindow(d, scr), 10, 10, 200, 200, 1,
    BlackPixel(d, scr), WhitePixel(d, scr));
  XSelectInput(d, w, KeyPressMask|KeyReleaseMask); XMapWindow(d, w);
  XEvent e, pe;
  fluid_settings_t *settings = nue(new_fluid_settings());
  fluid_synth_t *s = nue(new_fluid_synth(settings));
  fluid_synth_chorus_on(s, -1, 0); // 0 = FALSE; disable chorus all groups
  fluid_synth_reverb_on(s, -1, 0); // 0 = FALSE; disable reverb all groups
  noe(fluid_synth_sfload(s, "/home/danl/Documents/MuseScore2/Soundfonts/"
    "SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2", 1));
  nue(new_fluid_audio_driver(settings, s));
  int ns[109];
  for (int n = 0; n <= 108; n++) ns[n] = 0;
  XFlush(d);
  while(1){
    XNextEvent(d, &e); 
    switch(e.type){
    case KeyPress:
      n = e2n(e);
      if (!n) break;
      if (ns[n] == 1) break;
      //printf("on %d\n", n);
      fluid_synth_noteon(s, 0, n, 80);
      ns[n] = 1;
      break;
    case KeyRelease:
      if (XPending(d) > 0) {
        XPeekEvent(d, &pe);
        if (pe.type == KeyPress && pe.xkey.time == e.xkey.time &&
          pe.xkey.keycode == e.xkey.keycode) break;
      }
      n = e2n(e);
      if (!n) break;
      fluid_synth_noteoff(s, 0, n);
      ns[n] = 0;
      //printf("off %d\n", n);
    }
  }
}
