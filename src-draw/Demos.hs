module Demos (Demo(..), selectDemo, demoTournament) where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

data Demo
  = Tutorial Int
  | Trails Int
  | Vector Int

selectDemo :: Demo -> Diagram B R2
selectDemo (Tutorial n) = doSelectDemo demos n
selectDemo (Trails n) = doSelectDemo trailsDemos n
selectDemo (Vector n) = doSelectDemo vectorDemos n

demoTournament :: Diagram B R2
demoTournament = tournament 16

doSelectDemo :: [Diagram B R2] -> Int -> Diagram B R2
doSelectDemo ds n
  | and [0 < n, n <= l] = ds !! (n - 1)
  | otherwise = error $ concat ["Demo not defined (not in {1, 2.. ", show l, "})"]
  where
    l = length ds

demos :: [Diagram B R2]
demos =
  [ demoCircle
  , demoCircleStyled
  , demoTwoCircles
  , demoAtop
  , demoOrigin
  , demoOrigin2
  , demoSide
  , demoSideOrigin
  , demoBeside
  , demoHcat
  , demoEllipse
  , demoSnug
  , demoRotate
  , demoAlign
  ]

demoCircle :: Diagram B R2
demoCircle = circle 1

demoCircleStyled :: Diagram B R2
demoCircleStyled = circle 1 # fc blue # lw veryThick # lc purple # dashingG [0.2, 0.05] 0

demoAtop :: Diagram B R2
demoAtop = square 1 # fc aqua `atop` circle 1

-- show origins
demoOrigin :: Diagram B R2
demoOrigin = circle 1 # showOrigin

demoOrigin2 :: Diagram B R2
demoOrigin2 = demoAtop # showOrigin

-- stack horizontally
demoTwoCircles :: Diagram B R2
demoTwoCircles = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none

-- stack vertically
demoSide :: Diagram B R2
demoSide = circle 1 === square 2

-- origin is origin of first element
demoSideOrigin :: Diagram B R2
demoSideOrigin = demoSide # showOrigin

-- beside uses vector displacements (r2)
demoBeside :: Diagram B R2
demoBeside = beside (r2 (1,1)) (circle 1) (square 2)

-- horizontal concat (hcat is concat with |||) and strutX for spaces
demoHcat :: Diagram B R2
demoHcat = hcat [hcat [demoBeside, demoBeside], strutX 1, demoBeside ||| demoBeside]

-- ellipses. Since positioning is based on a separation line perp to the
-- center lines there is a gap in between
demoEllipse :: Diagram B R2
demoEllipse = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell ||| ell

-- solve the above gap issue
demoSnug :: Diagram B R2
demoSnug = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell # snugR <> ell # snugL

-- demo rotate by fraction of circle: rotate by 2pi/3 (120 degrees)
demoRotate :: Diagram B R2
demoRotate = square 1 # rotateBy (1/3)

-- demo align
demoAlign :: Diagram B R2
demoAlign = hrule (2 * sum sizes) === circles # centerX
  where circles = hcat . map alignB . zipWith scale sizes
                $ repeat (circle 1)
        sizes   = [2,5,4,7,1,3]

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map (node n) [1..]) #
  applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]
  where
    arrowOpts = with & gaps       .~ small
                     & headLength .~ Global 0.2

-- remember that <> is `atop`
node :: Int -> Int -> Diagram B R2
node m n
   = text (show n) # fontSizeN (0.5 / fromIntegral m) # fc white # translate (r2 (-0.01, -0.18))
  <> circle 0.2 # fc green # named n

vectorDemos :: [Diagram B R2]
vectorDemos =
  [ demoChainSaw
  , demoSemicircle
  , demoSpokes
  , vTriangle (unitX # rotateBy (3/7)) (unitX # rotateBy (1/8))
  , vAddRule (unitX # rotateBy (3/7)) (unitX # rotateBy (1/8))
  , demoPoints
  ]

-- fromOffsets takes a list of vectors to draw
-- r2 construct vectors from pairs
demoChainSaw :: Diagram B R2
demoChainSaw = fromOffsets . map r2 . zip (repeat 1) . map s $ [(1::Int)..10]
  where
    s x = if odd x then 1 else -1

-- fromDirection builds a vector on a direction (rad, turn or deg, use (@@) to
-- specify)
-- Also showcase fromDirection and scalar-vector multiplication
demoSemicircle :: Diagram B R2
demoSemicircle = mconcat . map build $ [-3, -2.. 3]
  where
    build = flip translate c . (5 *^) . fromDirection . (@@ deg) . (* 30)
    c = circle 1 # fc blue

-- list of lists: inner for spikes, outer for diagrams
demoSpokes :: Diagram B R2
demoSpokes = mconcat $ map buildOne [1..3]
  where
    s = 10
    buildOne x = rotate (x/3 @@ turn) $ buildStar x
    buildStar x = mconcat . map (f x) $ [1..s]
    f x y = fromOffsets [x *^ fromDirection (y/s @@ turn)]

-- vTriangle builds a triangle given two sides
vTriangle :: R2 -> R2 -> Diagram B R2
vTriangle a b = fromOffsets [a, b - a, -b]

-- vAddRule shows the vector addition rule
vAddRule :: R2 -> R2 -> Diagram B R2
vAddRule a b = mconcat
  [ fromOffsets [a + b] # vSum
  , mconcat $ build a b
  , mconcat $ build b a
  ]
  where
    build x y = [fromOffsets [x] # v, translate y $ fromOffsets [x] # vHlp]
    v         = lc blue
    vSum      = lc red
    vHlp      = lc purple # dashingG [0.2, 0.05] 0

-- use ^& but could also use p2
demoPoints :: Diagram B R2
demoPoints = position [(p, c p) | x <- l, y <- l, let p = (x ^& y)]
  where
    s = 31
    d = s ** 2
    l = [-s, -s+2..s]
    c p = circle 1 # fc (if distanceSq p origin < d then yellow else purple)

trailsDemos :: [Diagram B R2]
trailsDemos =
  [ demoBasicTrailFromOffsets
  , demoFromVertices
  , demoOnLineSegments
  ]

-- or map v2
demoBasicTrailFromOffsets :: Diagram B R2
demoBasicTrailFromOffsets = fromOffsets [(1 ^& 0), (0 ^& 2), (2 ^& 0)]

-- or map p2
demoFromVertices :: Diagram B R2
demoFromVertices = mconcat . map (flip translateX b) $ [1..4]
  where
    b = fromVertices [(0 ^& 0), (0 ^& 1), (1 ^& 0)]

-- strokeLine to convert a trail to diagram
demoOnLineSegments :: Diagram B R2
demoOnLineSegments = strokeLine . onLineSegments (drop 1) $ pentagon 1
