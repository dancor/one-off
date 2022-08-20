import os
import pyxhook
#import sofa as s
import sofa.read_sf2.read_sf2 as s
#import sf2_loader as s
#import read_sf2.read_sf2 as s
l = s.sf2_loader('/home/danl/Documents/MuseScore2/Soundfonts/SGM-v2.01-YamahaGrand-Guit-Bass-v2.7.sf2')

kToNote = {}

# Paul singing range: G2-G4

def onKeyUp(e):
    print("up: ", e.Key)
def onKeyDown(e):
    print("down", e.Key)
    note = ""
    if   e.Key == 'z': note = "C2"
    elif e.Key == 'x': note = "C#2"
    elif e.Key == 'c': note = "D2"
    elif e.Key == 'v': note = "D#2"
    elif e.Key == 'b': note = "E2"
    elif e.Key == 'k': note = "F2"
    elif e.Key == 'm': note = "F#2"
    elif e.Key == 'comma': note = "G2"
    elif e.Key == 'period': note = "G#2"
    elif e.Key == 'slash': note = "A2"
    elif e.Key == 'Shift_R': note = "A#2"
    elif e.Key == 'Up': note = "B2"
    elif e.Key == 'a': note = "C3"
    elif e.Key == 'r': note = "C#3"
    elif e.Key == 's': note = "D3"
    elif e.Key == 't': note = "D#3"
    elif e.Key == 'd': note = "E3"
    elif e.Key == 'h': note = "F3"
    elif e.Key == 'n': note = "F#3"
    elif e.Key == 'e': note = "G3"
    elif e.Key == 'i': note = "G#3"
    elif e.Key == 'o': note = "A3"
    #elif e.Key == 'apostrophe': note = "A#3"
    elif e.Key == 'Multi_key': note = "A#3"
    elif e.Key == 'Return': note = "B3"
    elif e.Key == 'q': note = "C4"
    elif e.Key == 'w': note = "C#4"
    elif e.Key == 'f': note = "D4"
    elif e.Key == 'p': note = "D#4"
    elif e.Key == 'g': note = "E4"
    elif e.Key == 'j': note = "F4"
    elif e.Key == 'l': note = "F#4"
    elif e.Key == 'u': note = "G4"
    elif e.Key == 'y': note = "G#4"
    elif e.Key == 'semicolon': note = "A4"
    elif e.Key == 'bracketleft': note = "A#4"
    elif e.Key == 'bracketright': note = "B4"
    elif e.Key == '1': note = "C5"
    elif e.Key == '2': note = "C#5"
    elif e.Key == '3': note = "D5"
    elif e.Key == '4': note = "D#5"
    elif e.Key == '5': note = "E5"
    elif e.Key == '6': note = "F5"
    elif e.Key == '7': note = "F#5"
    elif e.Key == '8': note = "G5"
    elif e.Key == '9': note = "G#5"
    elif e.Key == '0': note = "A5"
    elif e.Key == 'minus': note = "A#5"
    elif e.Key == 'equal': note = "B5"
    elif e.Key == 'F1': note = "C6"
    elif e.Key == 'F2': note = "C#6"
    elif e.Key == 'F3': note = "D6"
    elif e.Key == 'F4': note = "D#6"
    elif e.Key == 'F5': note = "E6"
    elif e.Key == 'F6': note = "F6"
    elif e.Key == 'F7': note = "F#6"
    elif e.Key == 'F8': note = "G6"
    elif e.Key == 'F9': note = "G#6"
    elif e.Key == 'F10': note = "A6"
    elif e.Key == 'F11': note = "A#6"
    elif e.Key == 'F12': note = "B6"
    elif e.Key == 'Pause': note = "C7"
    elif e.Key == 'Insert': note = "C#7"
    elif e.Key == 'Delete': note = "D7"
    elif e.Key == 'Home': note = "D#7"
    elif e.Key == 'Page_Up': note = "E7"
    elif e.Key == 'Next': note = "F7"
    elif e.Key == 'End': note = "F#7"
    elif e.Key == 'Right': note = "G7"
    elif e.Key == 'Down': note = "G#7"
    elif e.Key == 'Left': note = "A7"
    elif e.Key == 'Super_R': note = "A#7"
    elif e.Key == 'Control_R': note = "B7"
    elif e.Key == 'Alt_R': note = "C8"
    if note:
        #a = l.export_note(note)
        #s.play_sound(a)
        l.play_note(note)
  
new_hook = pyxhook.HookManager()
new_hook.KeyDown = onKeyDown
new_hook.KeyUp = onKeyUp
new_hook.HookKeyboard()
new_hook.start()
