{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Serialize
import Network.Pcap
import System.Environment

import Data.Enumerator hiding (map, filter, length, head)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
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
  --handle <- openLive "any" gSnapshotSize False 0
  handle <- openOffline "displayed.pcap"
  setFilter handle (buildFilter universe) True 0
  link <- datalink handle
  let hdrLen = linkHdrLen link
  putStrLn $ concat ["Capturing on ", universe]
  putStrLn "Press ^C to end"
  run_ $ iterateeChain handle hdrLen

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> LinkLength
linkHdrLen DLT_LINUX_SLL = 16 -- TODO: we should check that IP is next layer
linkHdrLen DLT_EN10MB = 14 -- TODO: same as above
linkHdrLen l = error $ concat ["Unknown link header ", show l]

iterateeChain :: PcapHandle -> LinkLength -> Iteratee CookedPacket IO ()
iterateeChain h hdrLen =
  packetEnumerator h $$
  removePayloadFail (DEL.mapM (dropCookedFrame hdrLen)) =$
  removePayloadFail (DEL.mapM processIP) =$
  removePayloadFail (DEL.mapM processTCP) =$
  removePayloadFail (DEL.mapAccumM processTCPConvs Map.empty) =$
  DEL.map updateSeqNo =$
  DEL.map sortPackets =$
  DEL.map removeDuplicates =$
  DEL.map filterForContent =$
  DEL.unique =$
  removePayloadFail (DEL.mapM processHTTP) =$
  removePayloadFail (DEL.mapM tagRequest) =$
  DEL.map extractURI =$
  DEL.map extractHTTPHeaders =$
  DEL.map parseHTTPHeaders =$
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
      state CloseACK _ = CloseACK -- succ CloseACK = undefined
      state s f = if TCPFIN `elem` f then succ s else s
      output mp port = let v = mp Map.! port in getOutput v
      getOutput (CloseACK, cv) = Just cv
      getOutput _ = Nothing

updateSeqNo :: [(TCP, Payload)] -> [(TCP, Payload)]
updateSeqNo l = map (\(t, p) -> (update t, p)) l
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) l
    reqSeq = fromMaybe 0 . listToMaybe . map (tcpSeqNr . fst) $ req
    ansSeq = fromMaybe 0 . listToMaybe . map (tcpSeqNr . fst) $ ans
    update t@TCP{..}
      | tcpSPort == gWebPort = t { tcpSeqNr = fix tcpSeqNr ansSeq,
                                   tcpAckNr = fix tcpAckNr reqSeq }
      | otherwise            = t { tcpSeqNr = fix tcpSeqNr reqSeq,
                                   tcpAckNr = fix tcpAckNr ansSeq }
    fix x y = if x > y then x - y else 0

sortPackets :: [(TCP, Payload)] -> [(TCP, Payload)]
sortPackets = sortBy (\(x, _) (y, _) -> x `compare` y)

removeDuplicates :: [(TCP, Payload)] -> [(TCP, Payload)]
removeDuplicates = nubBy (\(x, p) (y, q) -> x == y && B.length p == B.length q)

filterForContent :: [(TCP, Payload)] -> [(TCP, Payload)]
filterForContent = filter (\(_, x) -> B.length x > 0)

failPayload :: String -> IO (Maybe a)
failPayload s = putStrLn s >> return Nothing

processHTTP :: [(TCP, Payload)] -> IO (Maybe (Payload, Payload))
processHTTP l
  | length req > 1 = failPayload "One request only assumption failed"
  | otherwise = return $ Just (head $ map snd req, B.concat $ map snd ans)
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) l

tagRequest :: (Payload, Payload) -> IO (Maybe (HTTPRequestType, Payload, Payload))
tagRequest (req, resp)
  | rtype == "GET" = return $ Just (GET, B.tail rbody, resp)
  | otherwise = failPayload $ concat ["Unknown/unexpected request ", show rtype]
  where
    (rtype, rbody) = B.breakSubstring " " req

extractURI :: (HTTPRequestType, Payload, Payload) -> (HTTPRequestType, Payload, Payload, Payload)
extractURI (httpType, req, resp) = (httpType, B.tail uri, B.tail req', resp)
  where
    (uri, req') = B.breakSubstring " " req

extractHTTPHeaders :: (HTTPRequestType, Payload, Payload, Payload)
  -> (HTTPRequestType, Payload, Payload, Payload, Payload, Payload)
extractHTTPHeaders (httpType, uri, req, resp) = (httpType, uri, reqh, B.drop 4 req', resph, B.drop 4 resp')
  where
    (reqh, req') = B.breakSubstring "\r\n\r\n" req
    (resph, resp') = B.breakSubstring "\r\n\r\n" resp

parseHTTPHeaders :: (HTTPRequestType, Payload, Payload, Payload, Payload, Payload)
  -> (HTTPRequestType, Payload, [Header], Payload, [Header], Payload)
parseHTTPHeaders (httpType, uri, reqh, req, resph, resp)
  = (httpType, uri, hrq, req, hrsp, resp)
  where
    hrq = map B.tail $ tail $ C.split '\r' reqh
    hrsp = let w:ws = C.split '\r' resph in w : map B.tail ws

removePayloadFail :: Monad m =>
  Enumeratee a1 (Maybe a3) m (Step (Maybe a3) m (Step a3 m b)) ->
  Enumeratee a1 a3 m b
removePayloadFail ene = ene =$= DEL.filter isJust =$= DEL.map fromJust
