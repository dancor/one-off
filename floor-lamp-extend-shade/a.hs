data Pt
    = Pt Double Double
    deriving Show

data Circ
    = Circ
    { circCenter :: Pt
    , circRadius :: Double
    }
    deriving Show

circHighGivenPtXR :: Pt -> Double -> Double -> Circ
circHighGivenPtXR (Pt px py) ctrX r =
    Circ (Pt ctrX ctrY) r
  where
    ctrY = py + sqrt (r^2 - (px - ctrX)^2)

circXToAngle :: Circ -> Double -> Double
circXToAngle (Circ (Pt ctrX ctrY) r) x =
    acos $ (x - ctrX) / r

circAngleToPt :: Circ -> Double -> Pt
circAngleToPt (Circ (Pt ctrX ctrY) r) a =
    Pt (ctrX + r * cos a) (ctrY - r * sin a)

circNPtsAngleToAngle :: Circ -> Int -> Double -> Double -> [Pt]
circNPtsAngleToAngle circ n a1 a2 =
    map (circAngleToPt circ . (a1 +) . (((a2 - a1) / fromIntegral n) *) .
        fromIntegral) [0 .. n]

showPt :: Pt -> String
showPt (Pt x y) = show x ++ "," ++ show y

lampPath :: Pt -> [Pt] -> [Pt] -> [String]
lampPath (Pt x y) froms tos =
    ["  <path d=\"M" ++ show x ++ "," ++ show y
    ,"           a2438,2438 0 0,1 1016,0"
    ,"           l-26,122"
    ,"           a2313,2313 0 0,0 -964,0"
    ,"           l-26,-122"
    ,"          \""
    -- ,"        fill=\"#f1f1db\" stroke=\"#dddddd\" />"
    ,"        fill=\"#f1f1db\" stroke=\"#777777\" />"
--    ,"        fill=\"#f1f1db\" />"
    ,"  <path d=\"" ++
        concat (zipWith (\ from to -> 
                 " M" ++ showPt from ++ " L" ++ showPt to)
        (tail $ init froms) (tail $ init tos)) ++
        -- "\" stroke=\"#dddddd\" />"
        "\" stroke=\"#777777\" />"
{-
    ,"  <path d=\"" ++
        concat (zipWith (\ from to -> 
                 " M" ++ showPt from ++ " L" ++ showPt to)
        (head froms : [last froms]) (head tos : [last tos])) ++
        -- "\" stroke=\"#dddddd\" />"
        "\" stroke=\"#777777\" />"
-}
    ]

yAdd :: Double -> Pt -> Pt
yAdd dy (Pt x y) = Pt x (y + dy)

main = do
    let pt1 = Pt 2 59
        bigCirc = circHighGivenPtXR (Pt 2 59) (1016 / 2) 2438
        smCirc = bigCirc {circRadius = 2313}
        a1 = circXToAngle bigCirc 2
        a2 = circXToAngle bigCirc 1016
        pts1 = circNPtsAngleToAngle bigCirc 30 a1 a2
        pts2 = circNPtsAngleToAngle smCirc 30 a1 a2
    putStr . unlines $
        ["<?xml version=\"1.0\" standalone=\"no\"?>"
        ,"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
        ,"  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> "
        ,"<svg viewBox = \"0 0 1019 800\" version = \"1.1\">"
        ] ++
        concatMap (\ y -> lampPath (yAdd y pt1)
                   (map (yAdd y) pts1) (map (yAdd y) pts2))
                (map (* 154) [0..4]) ++
        ["</svg>"
        ]
