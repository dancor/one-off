from io import BytesIO
import musicpy as mp
import numpy as numpy
import os
from pydub import AudioSegment
import pyxhook
from sofa.read_sf2 import fluidsynth
import sofa.read_sf2.read_sf2 as s
from time import time, sleep
l = s.sf2_loader('/home/danl/Documents/MuseScore2/Soundfonts/' +
    'SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2')
kToNote = {}
# Paul singing range: G2-G4
def e2n(e):
    k = e.Key
    if k == 'z': return 36 # "C2"
    if k == 'x': return 37 # "C#2"
    if k == 'c': return 38 # "D2"
    if k == 'v': return 39 # "D#2"
    if k == 'b': return 40 # "E2"
    if k == 'k': return 41 # "F2"
    if k == 'm': return 42 # "F#2"
    if k == 'comma': return 43 # "G2"
    if k == 'period': return 44 # "G#2"
    if k == 'slash': return 45 # "A2"
    if k == 'Shift_R': return 46 # "A#2"
    if k == 'Up': return 47 # "B2"
    if k == 'a': return 48 # "C3
    if k == 'r': return 49 # "C#3"
    if k == 's': return 50 # "D3"
    if k == 't': return 51 # "D#3"
    if k == 'd': return 52 # "E3"
    if k == 'h': return 53 # "F3"
    if k == 'n': return 54 # "F#3"
    if k == 'e': return 55 # "G3"
    if k == 'i': return 56 # "G#3"
    if k == 'o': return 57 # "A3"
    #if k == 'apostrophe': return 58 # "A#3"
    if k == 'Multi_key': return 58 # "A#3"
    if k == 'Return': return 59 # "B3"
    if k == 'q': return 60 # "C4"
    if k == 'w': return 61 # "C#4"
    if k == 'f': return 62 # "D4"
    if k == 'p': return 63 # "D#4"
    if k == 'g': return 64 # "E4"
    if k == 'j': return 65 # "F4"
    if k == 'l': return 66 # "F#4"
    if k == 'u': return 67 # "G4"
    if k == 'y': return 68 # "G#4"
    if k == 'semicolon': return 69 # "A4"
    if k == 'bracketleft': return 70 # "A#4"
    if k == 'bracketright': return 71 # "B4"
    if k == '1': return 72 # "C5"
    if k == '2': return 73 # "C#5"
    if k == '3': return 74 # "D5"
    if k == '4': return 75 # "D#5"
    if k == '5': return 76 # "E5"
    if k == '6': return 77 # "F5"
    if k == '7': return 78 # "F#5"
    if k == '8': return 79 # "G5"
    if k == '9': return 80 # "G#5"
    if k == '0': return 81 # "A5"
    if k == 'minus': return 82 # "A#5"
    if k == 'equal': return 83 # "B5"
    if k == 'F1': return 84 # "C6"
    if k == 'F2': return 85 # "C#6"
    if k == 'F3': return 86 # "D6"
    if k == 'F4': return 87 # "D#6"
    if k == 'F5': return 88 # "E6"
    if k == 'F6': return 89 # "F6"
    if k == 'F7': return 90 # "F#6"
    if k == 'F8': return 91 # "G6"
    if k == 'F9': return 92 # "G#6"
    if k == 'F10': return 93 # "A6"
    if k == 'F11': return 94 # "A#6"
    if k == 'F12': return 95 # "B6"
    if k == 'Pause': return 96 # "C7"
    if k == 'Insert': return 97 # "C#7"
    if k == 'Delete': return 98 # "D7"
    if k == 'Home': return 99 # "D#7"
    if k == 'Page_Up': return 100 # "E7"
    if k == 'Next': return 101 # "F7"
    if k == 'End': return 102 # "F#7"
    if k == 'Right': return 103 # "G7"
    if k == 'Down': return 104 # "G#7"
    if k == 'Left': return 105 # "A7"
    if k == 'Super_R': return 106 # "A#7"
    if k == 'Control_R': return 107 # "B7"
    #if k == 'Alt_R': return 108 # "C8"
    if k == 'Alt_L': return 108 # "C8" # bug in my setup I guess
    return 0
ns = {}
def onKeyUp(e):
    n = e2n(e)
    if not n: return
    ns[n] = False
    l.synth.noteoff(0, n)
def onKeyDown(e):
    n = e2n(e)
    if not n: return
    if not n in ns or not ns[n]:
        l.synth.noteon(0, n, 100)
        ns[n] = True
hook = pyxhook.HookManager()
hook.KeyDown = onKeyDown
hook.KeyUp = onKeyUp
hook.HookKeyboard()
hook.start()
tGoal = time()
while True:
    tGoal += 0.2
    arrs = []
    arrs.append(l.synth.get_samples(44100))
    audArr = numpy.concatenate(arrs, axis=None)
    samps = fluidsynth.raw_audio_string(audArr)
    a = AudioSegment.from_raw(BytesIO(samps),
        sample_width=2, channels=2, frame_rate=44100)
    mp.pygame.mixer.Sound(buffer=a.raw_data).play()
    sleep(tGoal - time())
