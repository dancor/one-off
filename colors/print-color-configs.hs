{-# LANGUAGE QuasiQuotes #-}
import Data.List
import Data.List.Utils
import Text.RawString.QQ

bb :: [(String, String)]
bb = concat
    [ c "RED"     "#ff7c4d"
    , c "GREEN"   "#22ff00"
    , c "YELLOW"  "#ffcc00"
    , c "BLUE"    "#1a66ff"
    , c "MAGENTA" "#ff61df"
    , c "CYAN"    "#00ffff"
    ] where c x y = [('_':x ,y), ('_':'1':x ,y), ('_':'2':x ,y)]

ww :: [(String, String)]
ww = concat
    [ c "RED"     "#bd000d"
    , c "GREEN"   "#006607"
    , c "YELLOW"  "#ffbb00"
    , c "BLUE"    "#004ce6"
    , c "MAGENTA" "#ad007f"
    , c "CYAN"    "#005a61"
    ] where c x y = [('_':x ,y), ('_':'1':x ,y), ('_':'2':x ,y)]

main :: IO ()
main = mapM_ putStr [r ww s1, r ww s2, r bb s1, r bb s3] where
  r :: [(String, String)] -> String -> String
  r cols str = foldl' (\s (x, y) -> replace x y s) str cols

s1, s2, s3 :: String
-- Our 6 colors map to 8 normal Vim syntax highlighting categories, so 2 will
-- be non-bold to contrast: Chose Ignore and Type (only used in Vimscript?).
-- Blue faintest on black so only use it bold.
s1 = [r|
hi Comment    gui=bold guifg=_GREEN
hi Constant   gui=bold guifg=_RED
hi Error      gui=bold guifg=White guibg=_RED
hi Identifier gui=bold guifg=_CYAN
hi Ignore     gui=none guifg=_GREEN
hi PreProc    gui=bold guifg=_BLUE
hi Special    gui=bold guifg=_YELLOW
hi Statement  gui=bold guifg=_MAGENTA
hi Todo       gui=bold guifg=_BLUE guibg=_YELLOW
hi Type       gui=none guifg=_CYAN
|]; s2 = [r|
! DarkRed + Red
*color1:  _1RED
*color9:  _2RED
! DarkGreen + Green
*color2:  _1GREEN
*color10: _2GREEN
! DarkYellow + Yellow
*color3:  _1YELLOW
*color11: _2YELLOW
! DarkBlue + Blue
*color4:  _1BLUE
*color12: _2BLUE
! DarkMagenta + Magenta
*color5:  _1MAGENTA
*color13: _2MAGENTA
!DarkCyan + Cyan
*color6:  _1CYAN
*color14: _2CYAN
|]; s3 = [r|
        '--color1' , '_1RED', # DarkRed, Red
        '--color9' , '_2RED',
        '--color2' , '_1GREEN', # DarkGreen, Green
        '--color10', '_2GREEN',
        '--color3' , '_1YELLOW', # DarkYellow, Yellow
        '--color11', '_2YELLOW',
        '--color4' , '_1BLUE', # DarkBlue, Blue
        '--color12', '_2BLUE',
        '--color5' , '_1MAGENTA', # DarkMagenta, Magenta
        '--color13', '_2MAGENTA',
        '--color6' , '_1CYAN', # DarkCyan, Cyan
        '--color14', '_2CYAN',
|]
