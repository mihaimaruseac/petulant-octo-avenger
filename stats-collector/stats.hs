{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class
import Data.Maybe
import Data.Serialize
import Network.Pcap
import System.Environment

import Data.Enumerator hiding (map, filter)

import qualified Data.ByteString as B
import qualified Data.Enumerator.List as DEL
import qualified Data.Map.Strict as Map

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
  removePayloadFail (DEL.mapM (dropCookedFrame hdrLen)) =$
  removePayloadFail (DEL.mapM processIP) =$
  removePayloadFail (DEL.mapM processTCP) =$
  removePayloadFail (DEL.mapAccumM processTCPConvs Map.empty) =$
  -- TODO: We should check here for reordered frames, duplicate frames, duplicate chunks
  --DEL.map (\x -> map fst x) =$
  --DEL.map (\x -> (Prelude.length x, tcpSPort $ fst $ Prelude.head $ x)) =$
  printChunks False

packetEnumerator :: MonadIO m => PcapHandle -> Enumerator CookedPacket m b
packetEnumerator h = list
  where
    list (Continue k) = do
      pkt@(hdr, _) <- liftIO $ nextBS h
      k (Chunks $ if hdrCaptureLength hdr == 0 then [] else [pkt]) >>== list
    list step = returnI step

dropCookedFrame :: LinkLength -> CookedPacket -> IO (Maybe Payload)
dropCookedFrame hdrLen(PktHdr{..}, payload)
  | hdrWireLength <= hdrCaptureLength = return . Just $ B.drop hdrLen payload
  | otherwise = failPayload $
      concat ["Incomplete capture: ", show (hdrWireLength, hdrCaptureLength)]

processIP :: Payload -> IO (Maybe Payload)
processIP payload = case runGetPartial parseIP payload of
  Done ip p -> go ip p
  _ -> failPayload "Unhandled parseIP case"
  where
    go IPv4{..} p
      | ip4MFFlag == MoreFragments = failPayload
          "Unable to handle fragmentation at IP level"
      | ip4Proto == IPNextTCP = return $ Just p
      | otherwise = failPayload "Undefined layer 3 proto"

processTCP :: Payload -> IO (Maybe (TCP, Payload))
processTCP payload = case runGetPartial parseTCP payload of
  Done tcp p -> return $ Just (tcp, p)
  _ -> failPayload "Unhandled parseTCP case"

processTCPConvs :: Map.Map Port (TCPConversationState, TCPConversation) ->
  (TCP, Payload) ->
  IO (Map.Map Port (TCPConversationState, TCPConversation),
      Maybe TCPConversation)
processTCPConvs m c@(TCP{..}, _)
  | tcpSPort == gWebPort = update tcpDPort
  | tcpDPort == gWebPort = update tcpSPort
  | otherwise = do
      putStrLn $ concat ["Unknown port pair ", show (tcpSPort, tcpDPort)]
      return (m, Nothing)
    where
      update port = let m' = updateMap port in return (m', output m' port)
      updateMap port = Map.insertWith insertFun port (Ongoing, [c]) m
      insertFun (_, newc) (olds, oldc) = (state olds tcpFlags, oldc ++ newc)
      state CloseFinACK _ = CloseACK
      state s f = if TCPFIN `elem` f then succ s else s
      output mp port = let v = mp Map.! port in getOutput v
      getOutput (CloseACK, cv) = Just cv
      getOutput _ = Nothing

failPayload :: String -> IO (Maybe a)
failPayload s = putStrLn s >> return Nothing

removePayloadFail :: Monad m =>
  Enumeratee a1 (Maybe a3) m (Step (Maybe a3) m (Step a3 m b)) ->
  Enumeratee a1 a3 m b
removePayloadFail ene = ene =$= DEL.filter isJust =$= DEL.map fromJust

{-
process :: TCP -> Payload -> Payload
process TCP{..} p = do
  print (tcpSPort, tcpDPort, tcpFlags, tcpSeqNr, tcpAckNr)
  print p
-}
