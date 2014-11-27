-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Pcap
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
statsOn universe = do
  --handle <- openLive "any" gSnapshotSize False 0
  handle <- openOffline "displayed.pcap"
  setFilter handle (buildFilter universe) True 0
  link <- datalink handle
  let hdrLen = linkHdrLen link
  putStrLn $ "Capturing on " ++ universe
  putStrLn "Press ^C to end"
  processChain handle hdrLen

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> Int
linkHdrLen DLT_LINUX_SLL = 16 -- FUTURE: we should check that IP is next layer
linkHdrLen DLT_EN10MB = 14 -- FUTURE: same as above
linkHdrLen l = error $ "Unknown link header " ++ show l
