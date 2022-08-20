#include <fluidsynth.h>
#include <stdlib.h>
#include <unistd.h>
#include "err.h"
int main(int argc, char **argv){
  fluid_settings_t *sett = nue(new_fluid_settings());
  fluid_synth_t *s = nue(new_fluid_synth(sett));
  noe(fluid_synth_sfload(s, "/home/danl/Documents/MuseScore2/Soundfonts/"
    "SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2", 1));
  nue(new_fluid_audio_driver(sett, s));
  srand(getpid());
  int i, key;
  for(i = 0; i < 12; i++){
    key = 60 + (int)(12.0f * rand() / (float) RAND_MAX);
    fluid_synth_noteon(s, 0, key, 80);
    sleep(1);
    fluid_synth_noteoff(s, 0, key);
  }
}
