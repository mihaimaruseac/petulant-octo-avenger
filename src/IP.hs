{-
 - Parse IP header conforming to http://tools.ietf.org/html/rfc791
 -}
module IP (IP(..), DF(..), MF(..), IPNextProtocol(..), parseIP) where

import Control.Monad
import Data.Bits
import Data.Serialize
import Data.Word

data IP
  = IPv4
    { ip4Hlen :: Int
    , ip4Tos :: Word8 -- FUTURE
    , ip4Tlen :: Word16
    , ip4ID :: Word16
    , ip4DFFlag :: DF
    , ip4MFFlag :: MF
    , ip4FragOffset :: Word16
    , ip4TTL :: Word8
    , ip4Proto :: IPNextProtocol
    , ip4ChkSum :: Word16
    , ip4SrcAddr :: Word32 -- FUTURE
    , ip4DstAddr :: Word32 -- FUTURE
    }
  | IPv6
    { ip6TrafficClass :: Word8 -- FUTURE
    , ip6FlowLabel :: Integer
    , ip6PayloadLen :: Word16
    , ip6Proto :: IPNextProtocol
    , ip6HopLimit :: Word8 -- FUTURE
    , ip6SrcAddr :: Integer -- FUTURE
    , ip6DstAddr :: Integer -- FUTURE
    }
  deriving Show

data DF = MayFragment | DontFragment deriving (Eq, Show)
data MF = LastFragment | MoreFragments deriving (Eq, Show)
data IPNextProtocol = IPNextTCP deriving (Eq, Show)

parseIP :: Get IP
parseIP = do
  vhl <- getWord8
  let v = (vhl .&. 0xf0) `shiftR` 4
  let l = fromInteger . (4 *) . toInteger $ vhl .&. 0x0f
  case v of
    4 -> parseIPv4 l
    6 -> parseIPv6 l
    _ -> error $ concat ["IP version ", show v, " not recognized."]

parseIPv4 :: Int -> Get IP
parseIPv4 len = do
  tos <- getWord8
  tlen <- getWord16be
  idn <- getWord16be
  fo <- getWord16be
  when (fo .&. 0x8000 /= 0) $ error "Invalid IP frame: invalid reserved flag"
  let dfFlag = if fo .&. 0x4000 == 0 then MayFragment else DontFragment
  let mfFlag = if fo .&. 0x2000 == 0 then LastFragment else MoreFragments
  ttl <- getWord8
  p <- getWord8
  let proto = buildProtocol p
  chksum <- getWord16be
  src <- getWord32be
  dst <- getWord32be
  skip (len - 20) -- FUTURE: parse options
  return $ IPv4 len tos tlen idn dfFlag mfFlag (fo .&. 0x1fff) ttl proto chksum src dst

parseIPv6 :: Int -> Get IP
parseIPv6 rm = do
  let firstPart = fromInteger . toInteger $ rm `div` 4
  part <- getWord8
  labl <- getWord16be
  let secondPart = part .&. 0xf0
  let thirdPart = fromInteger . toInteger $ part .&. 0x0f
  let tClass = fromInteger . toInteger $ (firstPart `shiftL` 4) .|. (secondPart `shiftR` 4)
  let fLabel = toInteger $ (thirdPart `shiftR` 16) .|. labl
  plen <- getWord16be
  next <- getWord8
  let proto = buildProtocol next
  hop <- getWord8
  src <- readIP6Address
  dst <- readIP6Address
  return $ IPv6 tClass fLabel plen proto hop src dst

readIP6Address :: Get Integer
readIP6Address = do
  b1 <- getWord32be
  b2 <- getWord32be
  b3 <- getWord32be
  b4 <- getWord32be
  let combine a b = (a `shiftL` 32) .|. b
  return $ foldl combine 0 . map toInteger $ [b1, b2, b3, b4]

buildProtocol :: Word8 -> IPNextProtocol
buildProtocol 6 = IPNextTCP
buildProtocol x = error $ concat ["IP next protocol ", show x, " unknown."]
