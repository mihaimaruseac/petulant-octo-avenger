-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import System.Environment

main :: IO ()
main = firstDemo
{-do
  args <- getArgs
  case args of
    "1":_ -> firstDemo
    _ -> print args
    -}

firstDemo = mainWith (circle 1 :: Diagram B R2)
