module Demos where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

selectDemo :: Int -> Diagram B R2
selectDemo n
  | and [0 < n, n <= l] = demos !! (n - 1)
  | otherwise = error $ concat ["Demo not defined (not in {1, 2.. ", show l, "})"]
  where
    l = length demos

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
  -- TODO: move to own file
  ]
  where
    demoCircle = circle 1
    demoCircleStyled = circle 1 # fc blue # lw veryThick # lc purple # dashingG [0.2, 0.05] 0
    demoAtop = square 1 # fc aqua `atop` circle 1
    -- show origins
    demoOrigin = circle 1 # showOrigin
    demoOrigin2 = demoAtop # showOrigin
    -- stack horizontally
    demoTwoCircles = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none
    -- stack vertically
    demoSide = circle 1 === square 2
    -- origin is origin of first element
    demoSideOrigin = demoSide # showOrigin
    -- beside uses vector displacements (r2)
    demoBeside = beside (r2 (1,1)) (circle 1) (square 2)
    -- horizontal concat (hcat is concat with |||) and strutX for spaces
    demoHcat = hcat [hcat [demoBeside, demoBeside], strutX 1, demoBeside ||| demoBeside]
    -- ellipses. Since positioning is based on a separation line perp to the
    -- center lines there is a gap in between
    demoEllipse = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell ||| ell
    -- solve the above gap issue
    demoSnug = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell # snugR <> ell # snugL
    -- demo rotate by fraction of circle: rotate by 2pi/3 (120 degrees)
    demoRotate = square 1 # rotateBy (1/3)
    -- demo align
    demoAlign = hrule (2 * sum sizes) === circles # centerX
      where circles = hcat . map alignB . zipWith scale sizes
                    $ repeat (circle 1)
            sizes   = [2,5,4,7,1,3]

demoTournament :: Diagram B R2
demoTournament = tournament 16

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
