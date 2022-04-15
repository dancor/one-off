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
ESC:1 F1:59 .. F10:68 F11:87 F12:88 Pause:119 Ins:110 Del:111
~:41 One:2 .. BKSP:14 Home:102
TAB:15 .. ]:27 \:43 PGUP:104
CAPS:58 A:30 ':40 Enter:28 PGDN:109
Shift:42 Z:44 .. RSHIFT:54 UP:103 END:107
LCTRL:29 LWIN:126 LALT:56 Space:57 RAlt:100 RWin:127 RCtrl:97 Left:105 Down:108 Right:106
 */

#define len 2205 // 50 msec
#define preset 0 // piano, the main preset
#define vol 1.0f // piano, the main preset

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

  //tsf_note_on(sf, 0, 60, 1.0f); //preset 0, middle C
  tsf_note_on(sf, 0, 60, 1.0f); //preset 0, middle C
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

  while (true) {
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
      if (ev.type != 1) continue;
      if (ev.value == 1) { // value 1:down 2:holdRepeat 0:up
        int noteNum = ev.code;
        tsf_note_on(sf, preset, noteNum, vol);
        printf("d%d", noteNum); fflush(stdout);
      } else if (ev.value == 0) {
        int noteNum = ev.code;
        tsf_note_off(sf, preset, noteNum);
        printf("u%d\n", noteNum); fflush(stdout);
      }
    }
    usleep(10000);
    printf("."); fflush(stdout);
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
