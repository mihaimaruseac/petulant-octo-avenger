{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

type Demo = Int

main :: IO ()
main = mainWith selectDemo

selectDemo :: Demo -> Diagram B R2
selectDemo n
  | and [0 < n, n <= length demos] = demos !! (n - 1)
  | otherwise = error "Demo not defined"

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
