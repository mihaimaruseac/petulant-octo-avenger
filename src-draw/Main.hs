{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding ((<>))
import Options.Applicative hiding ((<>))

import qualified Diagrams.Prelude as D
import qualified Options.Applicative as O

type Demo = Int

data FlipOpts = FlipOpts Bool

instance Parseable FlipOpts where
  parser = FlipOpts <$> switch (long "flipped" O.<> help "Flip the diagram L-R")

main :: IO ()
main = mainWith (\(FlipOpts f) -> (if f then reflectX else id) $selectDemo 15) --mainWith selectDemo

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
  , demoBeside
  , demoHcat
  , demoEllipse
  , demoSnug
  , demoRotate
  , demoAlign
  , demoTournament -- TODO: move outside
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
    demoSnug = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell # snugR D.<> ell # snugL
    -- demo rotate by fraction of circle: rotate by 2pi/3 (120 degrees)
    demoRotate = square 1 # rotateBy (1/3)
    -- demo align
    demoAlign = hrule (2 * sum sizes) === circles # centerX
      where circles = hcat . map alignB . zipWith scale sizes
                    $ repeat (circle 1)
            sizes   = [2,5,4,7,1,3]

demoTournament :: Diagram B R2
demoTournament
  = tournament 16

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map (node n) [1..]) #
  applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]
  where
    arrowOpts = with & gaps       .~ small
                     & headLength .~ Global 0.2

-- remember that D.<> is `atop`
node :: Int -> Int -> Diagram B R2
node m n
   = text (show n) # fontSizeN (0.5 / fromIntegral m) # fc white # translate (r2 (-0.01, -0.18))
  D.<> circle 0.2 # fc green # named n
