{-
 - Parse TCP header conforming to http://tools.ietf.org/html/rfc793
 -}
module TCP (TCP(..), parseTCP, TCPFlags(..)) where

import Control.Monad
import Data.Bits
import Data.Serialize
import Data.Word

data TCP = TCP
  { tcpSPort :: Word16
  , tcpDPort :: Word16
  , tcpSeqNr :: Word32
  , tcpAckNr :: Word32
  , tcpDtOff :: Int
  , tcpFlags :: [TCPFlags]
  , tcpWindw :: Word16
  , tcpChkSm :: Word16
  , tcpUrgPt :: Word16
  } deriving Show

instance Eq TCP where
  t1 == t2 = and
    [ tcpSPort t1 == tcpSPort t2
    , tcpDPort t1 == tcpDPort t2
    , tcpSeqNr t1 == tcpSeqNr t2
    , tcpAckNr t1 == tcpAckNr t2
    , tcpFlags t1 == tcpFlags t2
    ]

data TCPFlags
  = TCPURG
  | TCPACK
  | TCPPSH
  | TCPRST
  | TCPSYN
  | TCPFIN
  deriving (Show, Eq, Enum)

parseTCP :: Get TCP
parseTCP = do
  sp <- getWord16be
  dp <- getWord16be
  sq <- getWord32be
  ack <- getWord32be
  fo <- getWord16be
  when (fo .&. 0x0fc0 /= 0) $ error "Invalid TCP packet: invalid reserved bits"
  let offset = fromIntegral $ 4 * ((fo .&. 0xf000) `shiftR` 12)
  let flags = getFlags (fo .&. 0x03f)
  w <- getWord16be
  ck <- getWord16be
  urg <- getWord16be
  skip (offset - 20) -- TODO: parse options
  return $ TCP sp dp sq ack offset flags w ck urg

getFlags :: Word16 -> [TCPFlags]
getFlags f = map fst . filter ((/= 0) . snd) . zip [TCPURG .. TCPFIN] . map ($ f) $ fields
  where
    fields = [(.&. 0x20), (.&. 0x10), (.&. 0x08), (.&. 0x04), (.&. 0x02), (.&. 0x01)]
