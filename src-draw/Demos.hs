module Demos (Demo(..), selectDemo, demoTournament) where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

data Demo
  = Tutorial Int
  | Vector Int

selectDemo :: Demo -> Diagram B R2
selectDemo (Tutorial n) = doSelectDemo demos n
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
