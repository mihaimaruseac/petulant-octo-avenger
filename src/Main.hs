-- {-# LANGUAGE RecordWildCards #-}
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

statsOn :: String -> IO ()
statsOn u = do
  putStrLn $ "Capturing on " ++ u
  putStrLn "Press ^C to end"
  processChain . buildFilter $ u

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]
