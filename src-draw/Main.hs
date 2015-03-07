-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1"] -> firstDemo
    _ -> print args

firstDemo = undefined
