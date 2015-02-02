{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessChain (statsOn) where

import Codec.Compression.GZip
import Control.Arrow
import Control.Exception
import Control.Monad.Error
import Data.Char
import Data.Conduit
import Data.List
import Data.Maybe
import Data.Serialize
import Network.Pcap
import Text.HTML.TagSoup

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Combinators as DCC
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Errors
import Globals
import IP
import TCP
import Tag
import Types

type ChanneledRequest = (HTTPRequestType, URI, RequestPayload, RequestPayload, ResponsePayload, ResponsePayload)
type CookedPacket = (PktHdr, Payload)
type HTTPTaggedRequest = (HTTPRequestType, RequestPayload, ResponsePayload)
type HTTPURIRequest = (HTTPRequestType, URI, RequestPayload, ResponsePayload)
type Request = (RequestPayload, ResponsePayload)
type TCPC = (TCPConversationState, TCPConversation)
type TCPConversation = [TCPPayload]
type TCPPayload = (TCP, Payload)

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)

statsOn :: String -> IO ()
statsOn u = do
  setupMsgs u
  bracket (openHandle u) fini work

work :: PcapHandle -> IO ()
work h = do
  link <- datalink h
  processChain h (linkHdrLen link)

fini :: PcapHandle -> IO ()
fini h = do
  stats <- statistics h
  print stats
  putStrLn "# Fini"

setupMsgs :: String -> IO ()
setupMsgs u = do
  putStrLn $ "# Capturing on " ++ u
  putStrLn "# Press ^C to end (and wait until next packet is captured)"

openHandle :: String -> IO PcapHandle
openHandle u = do
  h <- openLive "any" gSnapshotSize False 0
  setFilter h (buildFilter u) True 0
  return h

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> Int
linkHdrLen DLT_LINUX_SLL = 16 -- FUTURE: we should check that IP is next layer
linkHdrLen DLT_EN10MB = 14 -- FUTURE: same as above
linkHdrLen l = error $ "# Unknown link header " ++ show l

processChain :: PcapHandle -> Int -> IO ()
processChain h hdrLen = packetEnumerator h
  =$= DCC.map (dropCookedFrame hdrLen) =$= filterError
  =$= DCC.map processIP =$= filterError
  =$= DCC.map processTCP =$= filterError
  =$= DCC.concatMapAccum processTCPConvs Map.empty =$= filterError =$= CL.catMaybes
  =$= DCC.map updateSeqNo
  =$= DCC.map sortPackets =$= DCC.map removeDuplicates
  =$= DCC.map filterForContent =$= DCC.filter (/= []) =$= unique
  =$= DCC.map processHTTP =$= filterError
  =$= DCC.map tagRequest =$= filterError
  =$= DCC.map extractURI =$= filterError =$= CL.catMaybes
  =$= DCC.map extractHTTPHeaders
  =$= DCC.map parseHTTPHeaders
  =$= DCC.map gunzipBody =$= filterError
  =$= DCC.map tagHTML
  =$= DCC.map tagAndStore =$= filterError =$= DCC.concat
  $$  debugSink

debugSink :: Show i => Sink i IO ()
debugSink = CL.mapM_ print

packetEnumerator :: MonadIO m => PcapHandle -> Source m CookedPacket
packetEnumerator h = list
  where
    list = do
      pkt@(hdr, _) <- liftIO $ nextBS h
      if hdrCaptureLength hdr == 0 then list else yield pkt >> list

dropCookedFrame :: Int -> CookedPacket -> StatsM Payload
dropCookedFrame hdrLen (PktHdr{..}, payload)
  | hdrWireLength <= hdrCaptureLength = return $ B.drop hdrLen payload
  | otherwise = throwError $ IncompleteCapture hdrWireLength hdrCaptureLength

processIP :: Payload -> StatsM Payload
processIP payload = case runGetPartial parseIP payload of
  Done ip p -> go ip p
  _ -> throwError UnhandledParseIP
  where
    go IPv4{..} p
      | ip4MFFlag == MoreFragments = throwError FragmentationError
      | ip4Proto == IPNextTCP = return p
      | otherwise = throwError UndefinedLayer3Protocol
    go IPv6{..} p
      | ip6Proto == IPNextTCP = return p
      | otherwise = throwError UndefinedLayer3Protocol

processTCP :: Payload -> StatsM TCPPayload
processTCP payload = case runGetPartial parseTCP payload of
  Done tcp p -> return (tcp, p)
  _ -> throwError UnhandledParseTCP

processTCPConvs :: TCPPayload -> Map.Map Port TCPC -> (Map.Map Port TCPC, [StatsM (Maybe TCPConversation)])
processTCPConvs c@(TCP{..}, _) m
  | tcpSPort == gWebPort = update tcpDPort
  | tcpDPort == gWebPort = update tcpSPort
  | otherwise = (m, [throwError $ UnknownPortPair tcpSPort tcpDPort])
    where
      update port = let m' = updateMap port in (m', [output m' port])
      updateMap port = Map.insertWith insertFun port (Ongoing, [c]) m
      insertFun (_, newc) (olds, oldc) = (state olds tcpFlags, oldc ++ newc)
      state CloseFinACK _ = CloseACK
      state CloseACK _ = CloseACK -- EXPLAIN: succ CloseACK = undefined
      state s f = if TCPFIN `elem` f then succ s else s
      output mp port = let v = mp Map.! port in getOutput v
      getOutput (CloseACK, cv) = return $ Just cv
      getOutput _ = return Nothing

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

processHTTP :: TCPConversation -> StatsM Request
processHTTP conv
  | length req > 1 = throwError MoreRequestsInConversation
  | otherwise = return (head $ map snd req, B.concat $ map snd ans)
  where
    (req, ans) = partition (\(TCP{..}, _) -> tcpDPort == gWebPort) conv

tagRequest :: Request -> StatsM HTTPTaggedRequest
tagRequest (req, resp)
  | rtype == "GET" = return (GET, B.tail rbody, resp)
  | otherwise = throwError $ UnexpectedHTTPRequest rtype
  where
    (rtype, rbody) = B.breakSubstring " " req

extractURI :: HTTPTaggedRequest -> StatsM (Maybe HTTPURIRequest)
extractURI (httpType, req, resp)
  | "200 OK" `B.isSuffixOf` result = return $ Just (httpType, f uri, f req', B.drop 2 resp')
  | "302 Found" `B.isSuffixOf` result = return Nothing -- ignore 302 (redirection) error codes
  | otherwise = throwError $ HTTPError uri result
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

gunzipBody :: ChanneledHeaderRequest -> StatsM ChanneledHeaderRequest
gunzipBody (t, u, rh, rp, ah, ap)
  | h == "gzip" && h1 == "chunked" = return (t, u, rh, rp, ah, f ap)
  | otherwise = throwError $ UnacceptableEncoding h h1
  where
    h = searchHeader "Content-Encoding" ah
    h1 = searchHeader "Transfer-Encoding" ah
    f = BL.toStrict . decompress . BL.fromChunks . chunkify

tagHTML :: ChanneledHeaderRequest -> TaggedHeaderRequest
tagHTML (t, u, rh, rp, ah, ap) = (t, u, rh, rp, ah, sanitize $ parseTags ap)

sanitize :: [Tag Payload] -> [Tag Payload]
sanitize = filter (/= TagText "") . map sanitizeTag

sanitizeTag :: Tag Payload -> Tag Payload
sanitizeTag t
  | isTagText t = TagText . C.unwords . C.words . fromTagText $ t
  | otherwise = t

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

filterError :: ConduitM (StatsM o) o IO ()
filterError = do
  i <- await
  case i of
    Just (Right v) -> yield v >> filterError
    Just (Left e) -> lift (print e) >> filterError
    _ -> return ()


unique :: (Ord a, Monad m) => Conduit a m a
unique = DCC.concatMapAccum step Set.empty where
  step x s
    | x `Set.member` s = (s, [])
    | otherwise = (x `Set.insert` s, [x])
