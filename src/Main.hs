-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Globals
import ProcessChain

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> processChain gDefaultUniverse
    u:_ -> processChain u
