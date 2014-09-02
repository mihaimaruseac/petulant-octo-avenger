{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module IterateeChain (iterateeChain) where

import Codec.Compression.GZip
import Control.Monad.IO.Class
import Data.Char
import Data.Enumerator hiding (map, filter, length, head)
import Data.List
import Data.Maybe
import Data.Serialize
import Network.Pcap

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator.List as DEL
import qualified Data.Map.Strict as Map

import Globals
import IP
import Types
import TCP

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
  DEL.filter (/= []) =$
  DEL.unique =$
  removePayloadFail (DEL.mapM processHTTP) =$
  removePayloadFail (DEL.mapM tagRequest) =$
  removePayloadFail (DEL.mapM extractURI) =$
  DEL.map extractHTTPHeaders =$
  DEL.map parseHTTPHeaders =$
  removePayloadFail (DEL.mapM gunzipBody) =$
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

processTCP :: Payload -> IO (Maybe TCPPayload)
processTCP payload = case runGetPartial parseTCP payload of
  Done tcp p -> return $ Just (tcp, p)
  _ -> failPayload "Unhandled parseTCP case"

processTCPConvs :: Map.Map Port (TCPConversationState, TCPConversation) ->
  TCPPayload ->
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

updateSeqNo :: TCPConversation -> TCPConversation
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

sortPackets :: TCPConversation -> TCPConversation
sortPackets = sortBy (\(x, _) (y, _) -> x `compare` y)

removeDuplicates :: TCPConversation -> TCPConversation
removeDuplicates = nubBy (\(x, p) (y, q) -> x == y && B.length p == B.length q)

filterForContent :: TCPConversation -> TCPConversation
filterForContent = filter (\(_, x) -> B.length x > 0)

processHTTP :: TCPConversation -> IO (Maybe Request)
processHTTP l
  | length req > 1 = failPayload "One request only assumption failed"
  | otherwise = return $ Just (head $ map snd req, B.concat $ map snd ans)
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) l

tagRequest :: Request
  -> IO (Maybe HTTPTaggedRequest)
tagRequest (req, resp)
  | rtype == "GET" = return $ Just (GET, B.tail rbody, resp)
  | otherwise = failPayload $ concat ["Unknown/unexpected request ", show rtype]
  where
    (rtype, rbody) = B.breakSubstring " " req

extractURI :: HTTPTaggedRequest
  -> IO (Maybe HTTPURIRequest)
extractURI (httpType, req, resp)
  | "200 OK" `B.isSuffixOf` result = return $ Just (httpType, B.tail uri, B.tail req', B.drop 2 resp')
  | otherwise = failPayload $ concat ["Request to ", show uri, " failed with ", show result]
  where
    (uri, req') = B.breakSubstring " " req
    (result, resp') = B.breakSubstring "\r\n" resp

extractHTTPHeaders :: HTTPURIRequest
  -> ChanneledRequest
extractHTTPHeaders (httpType, uri, req, resp) = (httpType, uri, reqh, B.drop 4 req', resph, B.drop 4 resp')
  where
    (reqh, req') = B.breakSubstring "\r\n\r\n" req
    (resph, resp') = B.breakSubstring "\r\n\r\n" resp

parseHTTPHeaders :: ChanneledRequest
  -> ChanneledHeaderRequest
parseHTTPHeaders (httpType, uri, reqh, req, resph, resp)
  = (httpType, uri, fix hrq, req, fix hrsp, resp)
  where
    hrq = map B.tail $ tail $ C.split '\r' reqh
    hrsp = let w:ws = C.split '\r' resph in w : map B.tail ws
    fix = map (\(x, y) -> (x, B.drop 2 y)) . map (B.breakSubstring ": ")

gunzipBody :: ChanneledHeaderRequest
  -> IO (Maybe ChanneledHeaderRequest)
gunzipBody (t, u, rh, rp, ah, ap)
  | and [h == "gzip", h1 == "chunked"] = return $ Just (t, u, rh, rp, ah, BL.toStrict . decompress . BL.fromChunks . chunkify $ ap)
  | otherwise = failPayload $ concat ["Unacceptable encoding ", show h, " / ", show h1]
  where
    h = searchHeader "Content-Encoding" ah
    h1 = searchHeader "Transfer-Encoding" ah

chunkify :: Payload -> [Payload]
chunkify "" = []
chunkify payload
  | l /= 0 = ch : chunkify (B.drop 2 chs)
  | otherwise = []
  where
    (l1, p) = B.breakSubstring "\r\n" payload
    l = toChunkLen l1
    (ch, chs) = B.splitAt l . B.drop 2 $ p
    toChunkLen = B.foldl' (\s v -> 16 * s + lv (fromInteger . toInteger $ v)) 0
    lv x = if x <= ord '9' then x - ord '0' else 10 + x - ord 'a'

failPayload :: String -> IO (Maybe a)
failPayload s = putStrLn s >> return Nothing

removePayloadFail :: Monad m =>
  Enumeratee a1 (Maybe a3) m (Step (Maybe a3) m (Step a3 m b)) ->
  Enumeratee a1 a3 m b
removePayloadFail ene = ene =$= DEL.filter isJust =$= DEL.map fromJust
