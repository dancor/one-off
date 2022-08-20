from io import BytesIO
import musicpy as mp
import numpy as numpy
import os
from pydub import AudioSegment
import pyxhook
from sofa.read_sf2 import fluidsynth
import sofa.read_sf2.read_sf2 as s
l = s.sf2_loader('/home/danl/Documents/MuseScore2/Soundfonts/SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2')

kToNote = {}

# Paul singing range: G2-G4

def onKeyUp(e):
    print("up: ", e.Key)
def onKeyDown(e):
    print("down", e.Key)
    n = 0
    if   e.Key == 'z': n = 36 # "C2"
    elif e.Key == 'x': n = 37 # "C#2"
    elif e.Key == 'c': n = 38 # "D2"
    elif e.Key == 'v': n = 39 # "D#2"
    elif e.Key == 'b': n = 40 # "E2"
    elif e.Key == 'k': n = 41 # "F2"
    elif e.Key == 'm': n = 42 # "F#2"
    elif e.Key == 'comma': n = 43 # "G2"
    elif e.Key == 'period': n = 44 # "G#2"
    elif e.Key == 'slash': n = 45 # "A2"
    elif e.Key == 'Shift_R': n = 46 # "A#2"
    elif e.Key == 'Up': n = 47 # "B2"
    elif e.Key == 'a': n = 48 # "C3
    elif e.Key == 'r': n = 49 # "C#3"
    elif e.Key == 's': n = 50 # "D3"
    elif e.Key == 't': n = 51 # "D#3"
    elif e.Key == 'd': n = 52 # "E3"
    elif e.Key == 'h': n = 53 # "F3"
    elif e.Key == 'n': n = 54 # "F#3"
    elif e.Key == 'e': n = 55 # "G3"
    elif e.Key == 'i': n = 56 # "G#3"
    elif e.Key == 'o': n = 57 # "A3"
    #elif e.Key == 'apostrophe': n = 58 # "A#3"
    elif e.Key == 'Multi_key': n = 58 # "A#3"
    elif e.Key == 'Return': n = 59 # "B3"
    elif e.Key == 'q': n = 60 # "C4"
    elif e.Key == 'w': n = 61 # "C#4"
    elif e.Key == 'f': n = 62 # "D4"
    elif e.Key == 'p': n = 63 # "D#4"
    elif e.Key == 'g': n = 64 # "E4"
    elif e.Key == 'j': n = 65 # "F4"
    elif e.Key == 'l': n = 66 # "F#4"
    elif e.Key == 'u': n = 67 # "G4"
    elif e.Key == 'y': n = 68 # "G#4"
    elif e.Key == 'semicolon': n = 69 # "A4"
    elif e.Key == 'bracketleft': n = 70 # "A#4"
    elif e.Key == 'bracketright': n = 71 # "B4"
    elif e.Key == '1': n = 72 # "C5"
    elif e.Key == '2': n = 73 # "C#5"
    elif e.Key == '3': n = 74 # "D5"
    elif e.Key == '4': n = 75 # "D#5"
    elif e.Key == '5': n = 76 # "E5"
    elif e.Key == '6': n = 77 # "F5"
    elif e.Key == '7': n = 78 # "F#5"
    elif e.Key == '8': n = 79 # "G5"
    elif e.Key == '9': n = 80 # "G#5"
    elif e.Key == '0': n = 81 # "A5"
    elif e.Key == 'minus': n = 82 # "A#5"
    elif e.Key == 'equal': n = 83 # "B5"
    elif e.Key == 'F1': n = 84 # "C6"
    elif e.Key == 'F2': n = 85 # "C#6"
    elif e.Key == 'F3': n = 86 # "D6"
    elif e.Key == 'F4': n = 87 # "D#6"
    elif e.Key == 'F5': n = 88 # "E6"
    elif e.Key == 'F6': n = 89 # "F6"
    elif e.Key == 'F7': n = 90 # "F#6"
    elif e.Key == 'F8': n = 91 # "G6"
    elif e.Key == 'F9': n = 92 # "G#6"
    elif e.Key == 'F10': n = 93 # "A6"
    elif e.Key == 'F11': n = 94 # "A#6"
    elif e.Key == 'F12': n = 95 # "B6"
    elif e.Key == 'Pause': n = 96 # "C7"
    elif e.Key == 'Insert': n = 97 # "C#7"
    elif e.Key == 'Delete': n = 98 # "D7"
    elif e.Key == 'Home': n = 99 # "D#7"
    elif e.Key == 'Page_Up': n = 100 # "E7"
    elif e.Key == 'Next': n = 101 # "F7"
    elif e.Key == 'End': n = 102 # "F#7"
    elif e.Key == 'Right': n = 103 # "G7"
    elif e.Key == 'Down': n = 104 # "G#7"
    elif e.Key == 'Left': n = 105 # "A7"
    elif e.Key == 'Super_R': n = 106 # "A#7"
    elif e.Key == 'Control_R': n = 107 # "B7"
    elif e.Key == 'Alt_R': n = 108 # "C8"
    if n:
        l.synth.noteon(0, n, 100)
        whole_arrays = []
        whole_arrays.append(l.synth.get_samples(int(44100 * 2)))
        l.synth.noteoff(0, n)
        whole_arrays.append(l.synth.get_samples(int(44100 * 1)))
        audio_array = numpy.concatenate(whole_arrays, axis=None)
        current_samples = fluidsynth.raw_audio_string(audio_array)
        a = AudioSegment.from_raw(BytesIO(current_samples),
            sample_width=2, channels=2, frame_rate=44100)
        mp.pygame.mixer.Sound(buffer=a.raw_data).play()
  
new_hook = pyxhook.HookManager()
new_hook.KeyDown = onKeyDown
new_hook.KeyUp = onKeyUp
new_hook.HookKeyboard()
new_hook.start()
