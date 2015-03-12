{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1"] -> mainWith firstDemo
    _ -> print args
    --}
--firstDemo

firstDemo :: Diagram B R2
firstDemo = circle 1 -- :: Diagram B R2)
