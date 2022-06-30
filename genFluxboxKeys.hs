-- generate ~/.fluxbox/keys; todo? generate ~/bin/startupMyTerms
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
type I = Int; type T = T.Text; data Spot = Spot Char I I I
doSpot (Spot c' i' x' y') =
  "mod4 r " <> c <> " :exec xt -" <> (if even i' then "a" else "") <>
  "b -- -T a" <> i <> " -geometry 80x36+" <> x <> "+" <> y <> " -e sh -c s\\ "
  <> i <> "\n" <>
  --"mod4 m " <> c <> " :exec xt " <> (if even i' then "-a " else "") <>
  --"-- -T a" <> i <> " -geometry 80x36+" <> x <> "+" <> y <> " -e sh -c s\\ "
  -- <> i <> "\n" <>
  "mod4 t " <> c <> " :exec wmctrl -r :ACTIVE: -e 0," <> x <> "," <> y <>
  ",800,720\n"
  where f = T.pack . show; c = T.singleton c'; i = f i'; x = f x'; y = f y'
  --      w = f w'; h = f h'
main = T.putStr $ T.concat $ map doSpot [Spot 'a' 1 0 0, Spot 'r' 2 800 0,
  Spot 's' 3 1600 0, Spot 'n' 4 0 720, Spot 'e' 5 800 720, Spot 'i' 6 1600 720]
