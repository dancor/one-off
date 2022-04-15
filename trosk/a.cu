#define TSF_IMPLEMENTATION
#include <fcntl.h>
#include <linux/input.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <termios.h>
#include "tsf.h"
#include <unistd.h>
#include "/usr/include/alsa/asoundlib.h"

/*
inline uint64_t ticks() {
  uint32_t lo, hi;
  __asm__ __volatile__ (
    "xorl %%eax, %%eax\n"
    "cpuid\n"
    "rdtsc\n"
    : "=a" (lo), "=d" (hi)
    :
    : "%ebx", "%ecx");
  return (uint64_t)hi << 32 | lo;
}

ESC:1 F1:59 .. F10:68 F11:87 F12:88 Pause:119 Ins:110 Del:111
~:41 One:2 .. BKSP:14 Home:102
TAB:15 .. ]:27 \:43 PGUP:104
CAPS:58 A:30 ':40 Enter:28 PGDN:109
Shift:42 Z:44 .. RSHIFT:54 UP:103 END:107
LCTRL:29 LWIN:126 LALT:56 Space:57 RAlt:100 RWin:127 RCtrl:97 Left:105 Down:108 Right:106
 */

// timeStepSize = len / sampleRate
#define sampleRate 44100
#define len 55
#define timeStepSize 1.2471655328798186e-3
#define preset 0 // piano, the main preset
#define vol 1.0f // piano, the main preset

inline float getTime() {
  timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
}

int main(int argc, char **argv) {
  //tsf* sf = tsf_load_filename("/home/danl/Documents/MuseScore2/Soundfonts/EssentialKeysSforzandoV9.6.sf2");
  printf("Loading..\n");
  tsf* sf = tsf_load_filename("/home/danl/Documents/MuseScore2/Soundfonts/SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2");
  printf("Loaded.\n");
	/*for (int i = 0; i < sf->presetNum; i++) {
    auto p = sf->presets[i];
    printf("Preset %d: Number %d, Bank %d, Name %s\n", i, p.preset, p.bank, p.presetName);
  }*/
  tsf_set_output(sf, TSF_MONO, 44100, 0); //sample rate
  
  if(argc < 2) {
  	printf("usage: %s <device>\n", argv[0]);
  	return 1;
  }
  int fd = open(argv[1], O_RDONLY);
  printf("fd: %d\n", fd);
  struct pollfd fds[1];
  fds[0].fd = fd;
  fds[0].events = POLLIN;
  struct input_event ev;

  struct termios term;
  tcgetattr(fileno(stdin), &term);
  term.c_lflag &= ~ECHO;
  tcsetattr(fileno(stdin), 0, &term);

  float sound[len];

  int err;
  //int size_out;
  snd_pcm_t *sc; // soundcard
  snd_pcm_sframes_t frames;

  if ((err = snd_pcm_open(&sc, "default", SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    printf("Playback open error: %s\n", snd_strerror(err));
    exit(EXIT_FAILURE);
  }
  if ((err = snd_pcm_set_params(sc, SND_PCM_FORMAT_FLOAT_LE,
      SND_PCM_ACCESS_RW_INTERLEAVED, 1, 44100, 1, 250000)) < 0) {   
    printf("Playback open error: %s\n", snd_strerror(err));
    exit(EXIT_FAILURE);
  }

  int c, noteNum;
  while (true) {
    float t = getTime();
    tsf_render_float(sf, sound, len, 0);
    frames = snd_pcm_writei(sc, sound, len);
    if (frames < 0) frames = snd_pcm_recover(sc, frames, 0);
    if (frames < 0) {
      printf("snd_pcm_writei failed: %s\n", snd_strerror(err));
      snd_pcm_close(sc);
      return 0;
    }
    int pollRes = poll(fds, 1, 0);
    if (pollRes != 0) {
      if (pollRes == -1) perror("poll returned -1 which is bad");
      int lol = read(fd, &ev, sizeof(struct input_event));
      if (ev.type != 1 || ev.value == 2) goto passDone; // value 1:down 2:holdRepeat 0:up
      c = ev.code;
      noteNum = 
        c ==  44 ?  24 : // [Z]: C1
        c ==  45 ?  25 :
        c ==  46 ?  26 :
        c ==  47 ?  27 :
        c ==  48 ?  28 :
        c ==  49 ?  29 :
        c ==  50 ?  30 :
        c ==  51 ?  31 :
        c ==  52 ?  32 :
        c ==  53 ?  33 :
        c ==  54 ?  34 :
        c == 103 ?  35 :
        
        c ==  30 ?  36 : // [A]: C2
        c ==  31 ?  37 :
        c ==  32 ?  38 :
        c ==  33 ?  39 :
        c ==  34 ?  40 :
        c ==  35 ?  41 :
        c ==  36 ?  42 :
        c ==  37 ?  43 :
        c ==  38 ?  44 :
        c ==  39 ?  45 :
        c ==  40 ?  46 :
        c ==  28 ?  47 :
        
        c ==  16 ?  48 : // [Q]: C3
        c ==  17 ?  49 :
        c ==  18 ?  50 :
        c ==  19 ?  51 :
        c ==  20 ?  52 :
        c ==  21 ?  53 :
        c ==  22 ?  54 :
        c ==  23 ?  55 :
        c ==  24 ?  56 :
        c ==  25 ?  57 :
        c ==  26 ?  58 :
        c ==  27 ?  59 :

        c ==   2 ?  60 : // [1]: C4
        c ==   3 ?  61 :
        c ==   4 ?  62 :
        c ==   5 ?  63 :
        c ==   6 ?  64 :
        c ==   7 ?  65 :
        c ==   8 ?  66 :
        c ==   9 ?  67 :
        c ==  10 ?  68 :
        c ==  11 ?  69 :
        c ==  12 ?  70 :
        c ==  13 ?  71 :

        c ==  59 ?  72 : // [F1]: C5
        c ==  60 ?  73 :
        c ==  61 ?  74 :
        c ==  62 ?  75 :
        c ==  63 ?  76 :
        c ==  64 ?  77 :
        c ==  65 ?  78 :
        c ==  66 ?  79 :
        c ==  67 ?  80 :
        c ==  68 ?  81 :
        c ==  87 ?  82 :
        c ==  88 ?  83 :
        
        c == 119 ?  84 : // [Pause]: C6
        c == 110 ?  85 :
        c == 111 ?  86 :
        c == 102 ?  87 :
        c == 104 ?  88 :
        c == 109 ?  89 :
        c == 107 ?  90 :
        c == 106 ?  91 :
        c == 108 ?  92 :
        c == 105 ?  93 :
        c ==  97 ?  94 :
        c == 127 ?  95 :
        
        c == 100 ?  96 : // [RAlt]: C7

        /*
        c == 29  ? 24 :
        c == 126 ? 25 :
        c ==  56 ? 26 :
        c ==  57 ? 27 :
        c == 100 ? 28 :
        c == 127 ? 29 :
        c ==  97 ? 30 :
        c == 105 ? 31 :
        c == 108 ? 32 :
        c == 106 ? 33 :
        */
        23;
      if (noteNum == 23) {printf("%d!", c); fflush(stdout);} else tsf_note_on(sf, preset, noteNum, ev.value);
      //printf("%d:%d!", c, noteNum); fflush(stdout);
passDone:
      float tLeft = getTime() + timeStepSize - t;
      if (tLeft > 0) usleep(tLeft);
    }
    //printf("."); fflush(stdout);
  }
  //if (frames > 0 && frames < (long)sizeof(buffer))
  //  printf("Short write (expected %li, wrote %li)\n", (long)sizeof(buffer), frames);

  //snd_pcm_close(sc);
  //term.c_lflag |= ECHO;
  //tcsetattr(fileno(stdin), 0, &term);
}

/*
int main(int argc, char **argv) {
  while (1) {
    int lol = read(fd, &ev, sizeof(struct input_event));
    if (ev.type != 1) continue;
    // value 1:down 2:holdRepeat 0:up
    if (ev.value == 1) printf("on %i\n", ev.code);
  }
  
  //term.c_lflag |= ECHO;
  //tcsetattr(fileno(stdin), 0, &term);
}*/
