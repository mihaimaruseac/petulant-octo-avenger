{-# LANGUAGE RecordWildCards #-}

import Data.Serialize
import Network.Pcap
import System.Environment

import qualified Data.ByteString as B

import Globals
import IP
import TCP
import Types

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
linkHdrLen DLT_LINUX_SLL = 16 -- TODO we should check that IP is next layer
linkHdrLen l = error $ concat ["Unknown link header ", show l]

mainCallback :: LinkLength -> CallbackBS
mainCallback hdrLen h@PktHdr{..} payload
  | hdrWireLength > hdrCaptureLength = incomplete h payload
  | otherwise = processIP $ B.drop hdrLen payload

incomplete :: CallbackBS
incomplete header _ = putStrLn $ concat ["Incomplete packet captured ", show header]

processIP :: ProcessPacket
processIP payload = case runGetPartial parseIP payload of
  Done ip p -> go ip p
  _ -> error "Unhandled parseIP case"
  where
    go IPv4{..} p
      | ip4MFFlag == MoreFragments = error "Unable to handle fragmentation at IP level"
      | ip4Proto == IPNextTCP = processTCP p
      | otherwise = error "Undefined layer 3 proto"

processTCP :: ProcessPacket
processTCP payload = case runGetPartial parseTCP payload of
  Done tcp p -> process tcp p
  _ -> error "Unhandled parseTCP case"

process :: TCP -> ProcessPacket
process TCP{..} p = do
  print (tcpSPort, tcpDPort, tcpFlags, tcpSeqNr, tcpAckNr)
  print p
