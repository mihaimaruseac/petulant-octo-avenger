{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Pcap
import Numeric
import System.Environment

import qualified Data.ByteString as B

gSnapshotSize :: Int
gSnapshotSize = 1000000000

gDefaultUniverse :: String
gDefaultUniverse = "artemis"

type LinkLength = Integer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> statsOn gDefaultUniverse
    u:_ -> statsOn u

statsOn :: String -> IO ()
statsOn universe = do
  handle <- openLive "any" gSnapshotSize False 0
  setFilter handle (buildFilter universe) True 0
  link <- datalink handle
  let hdrLen = linkHdrLen link
  putStrLn $ concat ["Capturing on ", universe]
  putStrLn "Press ^C twice to end"
  pRead <- loopBS handle (- 1) $ mainCallback hdrLen
  putStrLn $ "Read " ++ show pRead ++ " packets"

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> LinkLength
linkHdrLen DLT_LINUX_SLL = 12
linkHdrLen l = error $ concat ["Unknown link header ", show l]

mainCallback :: LinkLength -> CallbackBS
mainCallback hdrLen h@PktHdr{..}
  | hdrWireLength > hdrCaptureLength = incomplete h
  | otherwise = process hdrLen h

incomplete :: CallbackBS
incomplete header _ = putStrLn $ "Incomplete packet captured" ++ show header

process :: LinkLength -> CallbackBS
process hdrLen header payload = do
    print header
    --print payload
    putStrLn . concatMap (flip showHex "") . B.unpack $ payload
