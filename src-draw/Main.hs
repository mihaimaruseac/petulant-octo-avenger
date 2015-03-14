{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
import System.Environment

import Diagrams.Prelude hiding ((<>))
import Options.Applicative

{-
data Selection
  = FirstDemo
  deriving (Eq, Show)

data FlipOpts = FlipOpts Bool

instance Parseable FlipOpts where
  parser = FlipOpts <$> switch (long "flipped" <> help "Flip the diagram L-R")

main :: IO ()
main = mainWith (\(FlipOpts f) -> (if f then reflectX else id) d)
{-do
  args <- getArgs
  mainWith $ genSelection args
  -}

genSelection :: a -> Selection
genSelection = undefined

{-
  case args of
    ["1"] -> mainWith firstDemo
    _ -> print args
    --}
--firstDemo

{-
switch :: Selection -> Diagram B R2
switch FirstDemo = firstDemo
-}
d = firstDemo
-}


type Demo = Int

main :: IO ()
main = mainWith selectDemo

selectDemo :: Demo -> Diagram B R2
selectDemo 1 = firstDemo
selectDemo 2 = secondDemo
selectDemo _ = error "Demo not defined"

firstDemo :: Diagram B R2
firstDemo = circle 1

secondDemo :: Diagram B R2
secondDemo = circle 1 # fc blue
                      # lw veryThick
                      # lc purple
                      # dashingG [0.2, 0.05] 0
