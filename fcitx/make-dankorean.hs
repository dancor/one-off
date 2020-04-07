ko_leads = ["g", "n", "d", "r", "m", "b", "s", "j", "c", "k", "t", "p", "h"]

ko_vowels = ["a", "ae", "ya", "yae", "eo", "e", "yeo", "ye", "o", "wa", "wae",
    "oe", "yo", "u", "weo", "we", "wi", "yu", "eu", "yi", "i"]

ko_tails = ["", "g", "gg", "gs", "n", "nj", "nh", "d", "l", "lg", "lm", "lb",
    "ls", "lt", "lp", "lh", "m", "b", "bs", "s", "ss", "ng", "j", "c", "k",
    "t", "p", "h"]

ko_unic = ['가' .. '힣']

main = do
    mapM_ putStrLn
      [ ";fcitx Version 0x03 Table file"
      , "KeyCode=abcdefghijklmnopqrstuvwxyz"
      , "Length=6"
      , "Pinyin=@"
      , "PinyinLength=6"
      , "Prompt=&"
      , "ConstructPhrase=^"
      , "[Data]"
      ]
    mapM_ putStrLn $
        zipWith (\romanization unic -> romanization <> " " <> [unic])
        (map concat $ sequence [ko_leads, ko_vowels, ko_tails]) ko_unic
