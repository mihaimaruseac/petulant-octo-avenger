{-# LANGUAGE RecordWildCards #-}

import Network.Pcap
import System.Environment

import qualified Data.ByteString as B

import Globals
import IP
import Types

import Debug.Trace

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
  putStrLn $ concat ["Read ", show pRead, " packets"]

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> LinkLength
linkHdrLen DLT_LINUX_SLL = 16
linkHdrLen l = error $ concat ["Unknown link header ", show l]

mainCallback :: LinkLength -> CallbackBS
mainCallback hdrLen h@PktHdr{..} payload
  | hdrWireLength > hdrCaptureLength = incomplete h payload
  | otherwise = process $ B.drop hdrLen payload

incomplete :: CallbackBS
incomplete header _ = putStrLn $ concat ["Incomplete packet captured ", show header]

process :: ProcessPacket
process = skipIP
