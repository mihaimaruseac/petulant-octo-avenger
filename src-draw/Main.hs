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
  ]
  where
    demoCircle = circle 1
    demoCircleStyled = circle 1 # fc blue # lw veryThick # lc purple # dashingG [0.2, 0.05] 0
    demoTwoCircles = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none
    demoAtop = square 1 # fc aqua `atop` circle 1
