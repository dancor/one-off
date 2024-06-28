#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <alsa/asoundlib.h>
#include "../../template/c/types.h"
#include "../../template/c/err.h"
#define sampleRate 44100
#define bufN 4410
#define pi 3.141592653589
#define myCard "hw:CARD=PCH" // Intel sound in its "Platform Controller Hub"
//#define myCard "hw:CARD=PCH,DEV=0"
// S16_LE format supported. S means signed. 16 means 2B.
// S32_LE format supported
// ~ cat /proc/asound/card0/codec#0:
//   rates [0x560]: 44100 48000 96000 192000
// intel cpus are also little-endian
#define runN 20
int main() {
  snd_pcm_t* pcm;
  nie(snd_pcm_open(&pcm, myCard, SND_PCM_STREAM_PLAYBACK, 0));
  //nie(snd_pcm_open(&pcm, myCard, SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK));
  nie(snd_pcm_set_params(pcm, SND_PCM_FORMAT_S16_LE,
    SND_PCM_ACCESS_RW_INTERLEAVED, 2, sampleRate, bufN, 0));
  f4t freq = 880.0;
  f4t xd = 2 * pi * freq / sampleRate;
  i2t *buf = nue(malloc(2 * bufN * sizeof(short)));
  int bufStartI = 0;
  /*for (int i = 0; i < bufN; i++, x += xd) {
    short val = 20000.0 * sin(x);
    buf[2*i] = val;
    buf[2*i+1] = val;
  }*/
  nie(snd_pcm_wait(pcm, -1));
  long j[runN];
  for (int i = 0; i < runN; i++) j[i] = 707;
  for (int i = 0; i < 20; i++) {
    for (int i = 0; i < bufN; i++) {
      //short val = 20000.0 * sin(xd * (bufStartI + i));
      (void)xd;
      short val = 256*(bufStartI + i);
      /*buf[2*i] = val;
      buf[2*i+1] = val;
      */
      //buf[2*i] = buf[2*i+1] = val;
      buf[2*i+1] = buf[2*i] = val;
    }
    j[i] = snd_pcm_writei(pcm, buf, bufN);
    if (j[i] == -EPIPE) snd_pcm_recover(pcm, EPIPE, 0);
    bufStartI += j[i];
    /*
    //j[i] = nie(snd_pcm_writei(pcm, buf, bufN));
    if (j[i] < sampleRate) {
      //zie(-EPIPE ==
      long ret = snd_pcm_writei(pcm, buf, bufN);
      printf("next write [%ld %d]\n", ret, -EPIPE == ret);
      nie(snd_pcm_recover(pcm, EPIPE, 0));
      nie(snd_pcm_wait(pcm, -1));
      //printf("Underrun.\n");
    }
    nie(j[i]);*/
  }
  for (int i = 0; i < runN; i++) {
    printf("j[%i] = %li\n", i, j[i]);
  }
  snd_pcm_close(pcm);
  free(buf);
  return 0;
  /*
  snd_pcm_t* pcm;
  nie(snd_pcm_open(&pcm, myCard, SND_PCM_STREAM_PLAYBACK, 0));
  snd_pcm_hw_params_t* hw;
  snd_pcm_hw_params_alloca(&hw);
  snd_pcm_hw_params_any(pcm, hw);
  for (int i=0; i<SND_PCM_FORMAT_LAST; i++) {
    if (snd_pcm_hw_params_test_format(pcm, hw, (snd_pcm_format_t)i) == 0) {
      printf("%s format supported\n",
        snd_pcm_format_name((snd_pcm_format_t)i));
    }
  }
  */
  /*
  snd_ctl_t *ctl; nie(snd_ctl_open(&ctl, myCard, 0));
  snd_ctl_card_info_t *info; nie(snd_ctl_card_info_malloc(&info));
  nie(snd_ctl_card_info(ctl, info));

  snd_ctl_card_info_free(info);
  nie(snd_ctl_close(ctl));
  */
}
