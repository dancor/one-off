module MemMajor where

import Data.List.Split

wds1 :: [String]
wds1 = [
  "SAW",   "YETI",   "HEN",  "HAM",   "OAR",   "WELL",  "WATCH", "EGG",     "WAVE",   "WHIP"
  ]

wds2 :: [String]
wds2 = [
  "SAUCE", "SEAWEED","SWAN", "SEAM",  "SEWER", "EASEL", "SAGE",  "SOCK",    "SAFE",   "SOAP",
  "DICE",  "TUTU",   "TEEN", "DAM",   "DOOR",  "DOLL",  "D.J.",  "DOG",     "TOFU",   "TUBA",
  "NOOSE", "KNIGHT", "NUN",  "GNOME", "NAIR",  "NAIL",  "NACHO", "NECK",    "KNIFE",  "KNOB",
  "MAZE",  "MOAT",   "MOON", "MUMMY", "MARIO", "MOLE",  "MATCH", "HAMMOCK", "MAFIA",  "MOP",
  "ROSE",  "RADIO",  "RHINO","WORM",  "AURORA","RAIL",  "RASH",  "ARK",     "REEF",   "ROBE",
  "LACE",  "L.E.D.", "LION", "LLAMA", "LAWYER","LILY",  "LUGE",  "LEGO",    "LAVA",   "LAB",
  "CHEESE","CHEETAH","GENIE","GYM",   "CHAIR", "CELLO", "JUDGE", "HEDGEHOG","CHEF",   "JEEP",
  "KAZOO", "CAT",    "CANE", "COMB",  "CHOIR", "KOALA", "CAGE",  "COOKIE",  "COFFEE", "COWBOY",
  "VISE",  "PHOTO",  "FAN",  "FOAM",  "FAIRY", "VALLEY","FISH",  "FIG",     "FIFE",   "FOB",
  "BUS",   "BAT",    "PIANO","BOMB",  "BRA",   "PILL",  "BADGE", "PIG",     "BEEHIVE","POPE"
  ]

intToWds :: Int -> [String]
intToWds x = 
  if length digits `mod` 2 == 1
    then (wds1 !! read [d0]) : f ds
    else f digits
  where
  digits@(d0:ds) = show x
  f = map ((wds2 !!) . read) . chunksOf 2
