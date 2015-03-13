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

firstDemo :: Diagram B R2
firstDemo = circle 1 -- :: Diagram B R2)

d = firstDemo
-}

main :: IO ()
main = mainWith (circle 1 :: Diagram B R2)
