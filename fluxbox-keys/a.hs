#include <h>
-- Mod4 codes:
-- b: black-make-terminal
-- m: make-terminal
-- r: run
-- t: travel-window-to
-- s: switch-focus
-- z: zap-invisible
type T = Text
main :: IO ()
--main = T.writeFile "/home/danl/.fluxbox/keys" $ T.unlines $ genCodes :
main = T.writeFile "out" $ T.unlines $ genCodes :
    map ("Mod4 " <>) mod4Codes ++ map ("Control Mod1 " <>) mod4Codes
sh :: Show a => a -> T
sh = T.pack . show
i2f :: Int -> Float
i2f = fromIntegral
ms, ss, ts :: [T]
ms = concatMap (\(a,s) -> [
  "m " <> a <> " :Exec multiplexed-positioned-titled-xt " <> s,
  "b " <> a <> " :Exec multiplexed-positioned-titled-xt -b " <> s]) [
  ("a", "1 0 a1"),
  ("r", "2 1 a2"),
  ("s", "3 2 a3"),
  ("t", "4 3 a4"),
  ("d", "5 4 a5"),
  ("h", "6 's2 0' b1"),
  ("n", "7 's2 1' b2"),
  ("e", "8 's2 2' b3"),
  ("i", "9 's2 3' b4"),
  ("o", "10 's2 4' b5")] 
ss = concatMap (ssH . T.singleton) ("abcdefghinoprstw" :: String) where
  ssH t = ["s " <> t <> " :Exec wf " <> t, "z " <> t <> " :Exec wf -z " <> t]
ts = map ("t "<>) $
  [ pre <> x2k x <> (if x < 15 then " " <> x2k w else "") <> wcmd x y w h
  | (pre, y, h) <- [("", 0, 16), ("q ", 0, 8), ("w ", 0, 14), ("p ", 8, 8)],
  x <- [0..15], w <- [1 .. 16 - x]]
  where
  x2k x = T.singleton $ "0123456789abcdef" !! (x `mod` 16)
  wcmd x y w h = " :Exec wmctrl -r :ACTIVE: -e 0," <>
    T.intercalate "," (map sh [160 * x, 90 * y, 160 * w, 90 * h])
{-
v 0 2 3 0 1 :Exec wmv 0      +0.333
v 0 2 3 1 1 :Exec wmv 0 0.333+0.334 # hack fit
v 0 2 3 2 1 :Exec wmv 0 0.666+0.335 # hack fit
v 0 2 3 0 2 :Exec wmv 0      +0.668 # hack fit
v 0 2 3 1 2 :Exec wmv 0 0.333+0.668 # hack fit
v 9 3 0 1 :Exec wmv 0+      +0.333
v 9 3 1 1 :Exec wmv 0+ 0.333+0.334 # hack fit
v 9 3 2 1 :Exec wmv 0+ 0.666+0.335 # hack fit
v 9 3 0 2 :Exec wmv 0+      +0.668 # hack fit
v 9 3 1 2 :Exec wmv 0+ 0.333+0.668 # hack fit
-}

mod4Codes :: [T]
mod4Codes = T.lines [r|d :ToggleDecor
x r :Reconfig
x m :RootMenu
x u :Exec unity-control-center
w :ClientMenu
F7 :Exec arst
n :Exec hide-notifier
r n :Exec show-notifier
a :Exec my-fcitx-remote -os danarabic  ; sleep 0.5; fid
j :Exec my-fcitx-remote -os mozc       ; sleep 0.5; fid
3 :Exec my-fcitx-remote -os ipa-x-sampa; sleep 0.5; fid
p :Exec my-fcitx-remote -os pinyin     ; sleep 0.5; fid
k :Exec my-fcitx-remote -os dankorean  ; sleep 0.5; fid
h :Exec my-fcitx-remote -os danhebrew  ; sleep 0.5; fid
c :Exec my-fcitx-remote -c             ; fid
F3 :Exec lower-brightness
F4 :Exec raise-brightness
F7 :Exec adjvol -m
F8 :Exec adjvol -l
F9 :Exec adjvol -r
F10 :Exec adjvol -s 70
r b :Exec torb 
r f :Exec ff -f           
r g :Exec ff -g           
r h :Exec ff -f -p 2     
r i :Exec ff -g -p 2
r j :Exec ff -f -p s
r k :Exec ff -g -p s
r x :Exec xt|] ++ ms ++ ss ++ ts

genCodes :: T
genCodes = [r|Mod1 Tab :NextWindow groups
Mod1 Shift Tab :PrevWindow groups
Mod1 F4 :Close
OnTitlebar Move1 :StartMoving
OnLeftGrip Move1 :StartResizing bottomleft
OnRightGrip Move1 :StartResizing bottomright
OnWindowBorder Move1 :StartMoving
OnTitlebar Mouse3 :WindowMenu
XF86AudioLowerVolume :Exec adjvol -l
XF86AudioRaiseVolume :Exec adjvol -r
XF86AudioMute :Exec adjvol -m
232 :Exec lower-brightness
233 :Exec raise-brightness
XF86Launch1 :Exec adjvol -s 70
OnDesktop Mouse1 :HideMenus
OnDesktop Mouse2 :WorkspaceMenu
OnDesktop Mouse3 :RootMenu
OnDesktop Mouse4 :NextWorkspace
OnDesktop Mouse5 :PrevWorkspace|]
