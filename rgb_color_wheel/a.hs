import Data.Fixed

data Pt
    = Pt
    { ptX :: Double
    , ptY :: Double
    }
    deriving Show

data Circ
    = Circ
    { circCenter :: Pt
    , circRadius :: Double
    }
    deriving Show

circAngleToPt :: Circ -> Double -> Pt
circAngleToPt (Circ (Pt ctrX ctrY) r) a =
    Pt (ctrX + r * cos a) (ctrY + r * sin a)

showPt :: Pt -> String
showPt (Pt x y) = show x ++ "," ++ show y

fillPoints :: String -> [Pt] -> [String]
fillPoints fillColor (pt:pts) =
    ["  <path d=\"M" ++ showPt pt ++
        concat [" L" ++ showPt p | p <- pts] ++
        "\" fill=\"" ++ fillColor ++ "\" />"]
fillPoints _ _ = error "fillPoints usage"

hsvToRgb :: Double -> Double -> Double -> String
hsvToRgb hue sat val =
    "rgb(" ++ show r'' ++ "%," ++ show g'' ++ "%," ++ show b''++ "%)"
  where
    c = val * sat
    h' = hue / 60
    x = c * (1 - abs (h' `mod'` 2 - 1))
    (r, g, b) =
        case floor h' :: Int of
          0 -> (c, x, 0)
          1 -> (x, c, 0)
          2 -> (0, c, x)
          3 -> (0, x, c)
          4 -> (x, 0, c)
          5 -> (c, 0, x)
          _ -> error "floor h'"
    m = val - c
    (r', g', b') = (r + m, g + m, b + m)
    (r'', g'', b'') = (100 * r', 100 * g', 100 * b')

levPetalCount :: Int -> Int
levPetalCount 1 = 6
levPetalCount n = 6 * 2 ^ (n - 2)

flowerLevelToHalfPetalAngle :: Int -> Double
flowerLevelToHalfPetalAngle n = pi / 6 / 2 ^ (n - 1)

flowerAngleToRadius :: Double -> Double
flowerAngleToRadius a = 1 / 2 / sin (5 * pi / 6 - a)

flowerPetal :: Double -> Int -> (String, [Pt])
flowerPetal pointAngle level = (,)
    (hsvToRgb (360 * pointAngle / 2 / pi) 1 1)
    $
    [ circAngleToPt (mainCirc {circRadius = innerRadius}) pointAngle
    , circAngleToPt mainCirc (pointAngle + a)
    , circAngleToPt (mainCirc {circRadius = fullR}) pointAngle
    , circAngleToPt mainCirc (pointAngle - a)
    ]
  where
    xOffset = 240
    yOffset = 240
    fullR = 240
    innerRadius =
        if level == 1
        then 0
        else fullR * flowerAngleToRadius
             (flowerLevelToHalfPetalAngle $ level - 1)
    a = flowerLevelToHalfPetalAngle level
    r = fullR * flowerAngleToRadius a
    mainCirc = Circ (Pt xOffset yOffset) r

main :: IO ()
main = do
    putStr . unlines $
        ["<?xml version=\"1.0\" standalone=\"no\"?>"
        ,"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
        ,"  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> "
        ,"<svg viewBox = \"0 0 480 480\" version = \"1.1\">"
        ] ++
        concatMap (\ lev ->
          concatMap (uncurry fillPoints)
          [ flowerPetal
            (2 * pi * (fromIntegral n - if lev == 1 then 1 else 0.5) /
                fromIntegral (levPetalCount lev))
            lev
          | n <- [1 .. levPetalCount lev]
          ])
          [1..10] ++
        ["</svg>"
        ]
