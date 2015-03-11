{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Globals
import ProcessChain

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> statsOn gDefaultUniverse
    u:_ -> statsOn u
