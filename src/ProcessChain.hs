{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessChain (statsOn) where

import Codec.Compression.GZip
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import Data.List
import Data.Maybe
import Data.Serialize
import Network.Pcap

-- import Data.Enumerator hiding (map, filter, length, head)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Combinators as DCC
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Globals
import IP
import TCP

type ChanneledHeaderRequest = (HTTPRequestType, URI, [RequestHeader], RequestPayload, [ResponseHeader], ResponsePayload)
type ChanneledRequest = (HTTPRequestType, URI, RequestPayload, RequestPayload, ResponsePayload, ResponsePayload)
type CookedPacket = (PktHdr, Payload)
type HTTPTaggedRequest = (HTTPRequestType, RequestPayload, ResponsePayload)
type HTTPURIRequest = (HTTPRequestType, URI, RequestPayload, ResponsePayload)
type Header = (HeaderType, HeaderValue)
type HeaderType = Payload
type HeaderValue = Payload
type Payload = B.ByteString
type Request = (RequestPayload, ResponsePayload)
type RequestHeader = Header
type RequestPayload = Payload
type ResponseHeader = Header
type ResponsePayload = Payload
type TCPC = (TCPConversationState, TCPConversation)
type TCPConversation = [TCPPayload]
type TCPPayload = (TCP, Payload)
type URI = Payload

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)

data HTTPRequestType = GET | POST
  deriving (Eq, Show, Ord, Enum)

statsOn :: String -> IO ()
statsOn u = do
  setupMsgs u
  h <- openHandle u
  link <- datalink h
  processChain h (linkHdrLen link)

setupMsgs :: String -> IO ()
setupMsgs u = do
  putStrLn $ "Capturing on " ++ u
  putStrLn "Press ^C to end"

openHandle :: String -> IO PcapHandle
openHandle u = do
  --handle <- openLive "any" gSnapshotSize False 0 -- TODO: the real one
  handle <- openOffline "displayed.pcap"
  setFilter handle (buildFilter u) True 0
  return handle

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> Int
linkHdrLen DLT_LINUX_SLL = 16 -- FUTURE: we should check that IP is next layer
linkHdrLen DLT_EN10MB = 14 -- FUTURE: same as above
linkHdrLen l = error $ "Unknown link header " ++ show l

processChain :: PcapHandle -> Int -> IO ()
processChain h hdrLen = id -- TODO: change to runResourceT??
  $   packetEnumerator h
  =$= removePayloadFail (DCC.mapM (dropCookedFrame hdrLen))
  =$= removePayloadFail (DCC.mapM processIP)
  =$= removePayloadFail (DCC.mapM processTCP)
  =$= DCC.concatMapAccumM processTCPConvs Map.empty
  =$= DCC.map updateSeqNo
  =$= DCC.map sortPackets
  =$= DCC.map removeDuplicates
  =$= DCC.map filterForContent
  =$= DCC.filter (/= [])
  =$= unique
  $$  debugSink

{-
  removePayloadFail (DEL.mapM processHTTP) =$
  removePayloadFail (DEL.mapM tagRequest) =$
  removePayloadFail (DEL.mapM extractURI) =$
  DEL.map extractHTTPHeaders =$
  DEL.map parseHTTPHeaders =$
  removePayloadFail (DEL.mapM gunzipBody) =$
  printChunks False
  -}

debugSink :: Show i => Sink i IO ()
debugSink = CL.mapM_ print

packetEnumerator :: MonadIO m => PcapHandle -> Source m CookedPacket
packetEnumerator h = list
  where
    list = do
      pkt@(hdr, _) <- liftIO $ nextBS h
      if hdrCaptureLength hdr == 0 then list else yield pkt >> list

dropCookedFrame :: Int -> CookedPacket -> IO (Maybe Payload)
dropCookedFrame hdrLen (PktHdr{..}, payload)
  | hdrWireLength <= hdrCaptureLength = return . Just $ B.drop hdrLen payload
  | otherwise = failPayload $ "Incomplete capture: " ++ show (hdrWireLength, hdrCaptureLength)

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

processTCPConvs :: TCPPayload -> Map.Map Port TCPC -> IO (Map.Map Port TCPC, [TCPConversation])
processTCPConvs c@(TCP{..}, _) m
  | tcpSPort == gWebPort = update tcpDPort
  | tcpDPort == gWebPort = update tcpSPort
  | otherwise = do
      putStrLn $ "Unknown port pair " ++ show (tcpSPort, tcpDPort)
      return (m, [])
    where
      update port = let m' = updateMap port in return (m', output m' port)
      updateMap port = Map.insertWith insertFun port (Ongoing, [c]) m
      insertFun (_, newc) (olds, oldc) = (state olds tcpFlags, oldc ++ newc)
      state CloseFinACK _ = CloseACK
      state CloseACK _ = CloseACK -- succ CloseACK = undefined
      state s f = if TCPFIN `elem` f then succ s else s
      output mp port = let v = mp Map.! port in getOutput v
      getOutput (CloseACK, cv) = [cv]
      getOutput _ = []

updateSeqNo :: TCPConversation -> TCPConversation
updateSeqNo conv = map (first update) conv
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) conv
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
processHTTP conv
  | length req > 1 = failPayload "One request only assumption failed"
  | otherwise = return $ Just (head $ map snd req, B.concat $ map snd ans)
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) conv

tagRequest :: Request -> IO (Maybe HTTPTaggedRequest)
tagRequest (req, resp)
  | rtype == "GET" = return $ Just (GET, B.tail rbody, resp)
  | otherwise = failPayload $ "Unknown/unexpected request " ++ show rtype
  where
    (rtype, rbody) = B.breakSubstring " " req

extractURI :: HTTPTaggedRequest -> IO (Maybe HTTPURIRequest)
extractURI (httpType, req, resp)
  | "200 OK" `B.isSuffixOf` result = return $ Just (httpType, f uri, f req', B.drop 2 resp')
  | otherwise = failPayload $ concat ["Request to ", show uri, " failed with ", show result]
  where
    (uri, req') = B.breakSubstring " " req
    (result, resp') = B.breakSubstring "\r\n" resp
    f = B.tail

extractHTTPHeaders :: HTTPURIRequest -> ChanneledRequest
extractHTTPHeaders (httpType, uri, req, resp) = (httpType, uri, reqh, f req', resph, f resp')
  where
    (reqh, req') = B.breakSubstring "\r\n\r\n" req
    (resph, resp') = B.breakSubstring "\r\n\r\n" resp
    f = B.drop 4

parseHTTPHeaders :: ChanneledRequest -> ChanneledHeaderRequest
parseHTTPHeaders (httpType, uri, reqh, req, resph, resp)
  = (httpType, uri, fix hrq, req, fix hrsp, resp)
  where
    hrq = map B.tail $ tail $ C.split '\r' reqh
    hrsp = let w:ws = C.split '\r' resph in w : map B.tail ws
    fix = map (second (B.drop 2) . B.breakSubstring ": ")

gunzipBody :: ChanneledHeaderRequest -> IO (Maybe ChanneledHeaderRequest)
gunzipBody (t, u, rh, rp, ah, ap)
  | h == "gzip" && h1 == "chunked" = return $ Just (t, u, rh, rp, ah, f ap)
  | otherwise = failPayload $ concat ["Unacceptable encoding ", show h, " / ", show h1]
  where
    h = searchHeader "Content-Encoding" ah
    h1 = searchHeader "Transfer-Encoding" ah
    f = BL.toStrict . decompress . BL.fromChunks . chunkify

searchHeader :: Payload -> [Header] -> Payload
searchHeader h hs = fromMaybe "" $ lookup h hs

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

removePayloadFail :: Monad m => ConduitM i (Maybe o) m r -> ConduitM i o m r
removePayloadFail = mapOutputMaybe id

unique :: (Ord a, Monad m) => Conduit a m a
unique = DCC.concatMapAccum step Set.empty where
  step x s
    | x `Set.member` s = (s, [])
    | otherwise = (x `Set.insert` s, [x])
