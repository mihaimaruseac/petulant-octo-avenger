{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class
import Data.Serialize
import Network.Pcap
import System.Environment

import Data.Enumerator hiding (map, filter)

import qualified Data.ByteString as B
import qualified Data.Enumerator.List as DEL

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
  putStrLn "Press ^C to end"
  run_ $ iterateeChain handle hdrLen

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> LinkLength
linkHdrLen DLT_LINUX_SLL = 16 -- TODO we should check that IP is next layer
linkHdrLen l = error $ concat ["Unknown link header ", show l]

iterateeChain :: PcapHandle -> LinkLength -> Iteratee CookedPacket IO ()
iterateeChain h hdrLen =
  packetEnumerator h $$
  DEL.mapM (dropCookedFrame hdrLen) =$ DEL.filter (not . B.null) =$
  DEL.map processIP =$
  DEL.map processTCP =$
  printChunks False

packetEnumerator :: MonadIO m => PcapHandle -> Enumerator CookedPacket m b
packetEnumerator h = list
  where
    list (Continue k) = do
      pkt@(hdr, _) <- liftIO $ nextBS h
      k (Chunks $ if hdrCaptureLength hdr == 0 then [] else [pkt]) >>== list
    list step = returnI step

dropCookedFrame :: LinkLength -> CookedPacket -> IO Payload
dropCookedFrame hdrLen(PktHdr{..}, payload)
  | hdrWireLength <= hdrCaptureLength = return $ B.drop hdrLen payload
  | otherwise = do
      putStrLn $ concat ["Incomplete capture: ", show (hdrWireLength, hdrCaptureLength)]
      return B.empty

processIP :: Payload -> Payload
processIP payload = case runGetPartial parseIP payload of
  Done ip p -> go ip p
  _ -> error "Unhandled parseIP case"
  where
    go IPv4{..} p
      | ip4MFFlag == MoreFragments = error "Unable to handle fragmentation at IP level"
      | ip4Proto == IPNextTCP = p
      | otherwise = error "Undefined layer 3 proto"

processTCP :: Payload -> (TCP, Payload)
processTCP payload = case runGetPartial parseTCP payload of
  Done tcp p -> (tcp, p)
  _ -> error "Unhandled parseTCP case"

{-
process :: TCP -> Payload -> Payload
process TCP{..} p = do
  print (tcpSPort, tcpDPort, tcpFlags, tcpSeqNr, tcpAckNr)
  print p
-}
