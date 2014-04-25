{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Pcap
import System.Environment

gSnapshotSize :: Int
gSnapshotSize = 1000000000

gDefaultUniverse :: String
gDefaultUniverse = "artemis"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> statsOn gDefaultUniverse
    u:_ -> statsOn u

statsOn :: String -> IO ()
statsOn universe = do
  putStrLn $ concat ["Capturing on ", universe]
  putStrLn "Press ^C twice to end"
  handle <- openLive "any" gSnapshotSize False 0
  setFilter handle (buildFilter universe) True 0
  pRead <- loopBS handle (- 1) mainCallback
  putStrLn $ "Read " ++ show pRead ++ " packets"

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

mainCallback :: CallbackBS
mainCallback h@PktHdr{..}
  | hdrWireLength > hdrCaptureLength = incomplete h
  | otherwise = process h

incomplete :: CallbackBS
incomplete header _ = putStrLn $ "Incomplete packet captured" ++ show header

process :: CallbackBS
process header payload = do
    print header
    --print payload
