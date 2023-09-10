#include <h>
-- b: brightTerm
-- g: geom
-- r: run
-- s: switchFocus
-- t: takeTo
-- z: zapInvisible
type T = Text
main :: IO ()
--main = T.writeFile "/home/danl/.fluxbox/keys" $ T.unlines $ genCodes :
main = T.writeFile "out" $ T.unlines $ genCodes :
    map ("Mod4 " <>) mod4Codes -- ++ map ("Control Mod1 " <>) mod4Codes
sh :: Show a => a -> T
sh = T.pack . show
i2f :: Int -> Float
i2f = fromIntegral
genCodes :: T
genCodes = [r|232 :Exec lower-brightness
233 :Exec raise-brightness
Mod1 F4 :Close
Mod1 Tab :NextWindow groups
Modl Shift Tab :PrevWindow groups
OnDesktop Mouse1 :HideMenus
OnDesktop Mouse2 :WorkspaceMenu
OnDesktop Mouse3 :RootMenu
OnDesktop Mouse4 :NextWorkspace
OnDesktop Mouse5 :PrevWorkspace
OnLeftGrip Move1 :StartResizing bottomleft
OnRightGrip Move1 :StartResizing bottomright
OnTitlebar Mouse3 :WindowMenu
OnTitlebar Move1 :StartMoving
OnWindowBorder Move1 :StartMoving
XF86AudioLowerVolume :Exec adjvol -l
XF86AudioMute :Exec adjvol -m
XF86AudioRaiseVolume :Exec adjvol -r
XF86Launch1 :Exec adjvol -s 70|]
mod4Codes :: [T]
mod4Codes = T.lines [r|d :ToggleDecor
F10 :Exec adjvol -s 70
F3 :Exec lower-brightness
F4 :Exec raise-brightness
F7 :Exec adjvol -m
F7 :Exec arst
F8 :Exec adjvol -l
F9 :Exec adjvol -r
n :Exec hide-notifier
w :ClientMenu
x m :RootMenu
x r :Reconfig
x u :Exec unity-control-center
b a :exec runTerm1
b r :exec runTerm2
b s :exec runTerm3
b n :exec runTerm4
b e :exec runTerm5
b i :exec runTerm6
b x :Exec runBigTerm1
b y :Exec runBigTerm2
b z :Exec runBigTerm3
r a :exec runNiteTerm1
r r :exec runNiteTerm2
r s :exec runNiteTerm3
r n :exec runNiteTerm4
r e :exec runNiteTerm5
r i :exec runNiteTerm6
r x :Exec runBigNiteTerm1
r y :Exec runBigNiteTerm2
r z :Exec runBigNiteTerm3
r b :Exec torb 
r c :Exec signal
r f :Exec firefox
r g :Exec azgo
r o :Exec show-notifier
r Shift x :Exec xterm
s a :Exec wmctrl -a pbydv1 # ars nei t
s b :Exec wmctrl -a Tor\ Browser
s c :Exec wmctrl -a signal
s e :Exec wmctrl -a pbydv5
s f :Exec ~/p/dancomp/c/focusWeb f
s g :Exec ~/p/dancomp/c/focusWeb g
s h :Exec ~/p/dancomp/c/focusWeb h
s i :Exec wmctrl -a pbydv6
s n :Exec wmctrl -a pbydv4
s o :Exec wmctrl -a pbydvn
s p :Exec wmctrl -a evince
s r :Exec wmctrl -a pbydv2
s s :Exec wmctrl -a pbydv3
s t :Exec wmctrl -a pbydv7
s x :Exec wmctrl -a pbydvx
s y :Exec wmctrl -a pbydvy
s z :Exec wmctrl -a pbydvz
t a :exec wmctrl -r :ACTIVE: -e 0,0,0,800,720
t r :exec wmctrl -r :ACTIVE: -e 0,800,0,800,720
t s :exec wmctrl -r :ACTIVE: -e 0,1600,0,800,720
t n :exec wmctrl -r :ACTIVE: -e 0,0,720,800,720
t e :exec wmctrl -r :ACTIVE: -e 0,800,720,800,720
t i :exec wmctrl -r :ACTIVE: -e 0,1600,720,800,720
t z :Exec wmv 0
t q z :Exec wmv 0 +.5
t w z :Exec wmv 0 +.87
t f z :Exec wmv 0 .5+
t x :Exec wmv 1
t q x :Exec wmv 1 +.5
t w x :Exec wmv 1 +.87
t f x :Exec wmv 1 .5+
t c :Exec wmv 2
t q c :Exec wmv 2 +.5
t w c :Exec wmv 2 +.87
t f c :Exec wmv 2 .5+
t v :Exec wmv 3
t q v :Exec wmv 3 +.5
t w v :Exec wmv 3 +.87
t f v :Exec wmv 3 .5+
t b :Exec wmv 4+
t q b :Exec wmv 4+ +.5
t w b :Exec wmv 4+ +.87
t f b :Exec wmv 4+ .5+|] ++
  ["t " <> pre <> x2k x <> (if x < 15 then " " <> x2k w else "") <>
  wcmd x y w h | (pre, y, h) <- [("", 0, 16), ("q ", 0, 8), ("w ", 0, 14),
  ("p ", 8, 8)], x <- [0..15], w <- [1 .. 16 - x]] ++ 
  [ "g " <> x2k x <> " " <> x2k y <> (if x < 15 || y < 15 then " " <> x2k w <>
  (if y < 15 then " " <> x2k h else "") else "") <> wcmd x y w h
  | x <- [0..15], y <- [0..15], w <- [1 .. 16 - x], h <- [1 .. 16 - y]] where
  x2k x = T.singleton $ "0123456789abcdef" !! (x `mod` 16)
  wcmd x y w h = " :Exec wmctrl -r :ACTIVE: -e 0," <>
    T.intercalate "," (map sh [160 * x, 90 * y, 160 * w, 90 * h])
