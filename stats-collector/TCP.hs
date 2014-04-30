{-
 - Parse TCP header conforming to http://tools.ietf.org/html/rfc793
 -}
module TCP (TCP(..), parseTCP) where

import Control.Monad
import Data.Bits
import Data.Serialize
import Data.Word

data TCP = TCP
  { tcpSPort :: Word16
  , tcpDPort :: Word16
  , tcpSeqNr :: Word32
  , tcpAckNr :: Word32
  } deriving Show

parseTCP :: Get TCP
parseTCP = do
  sp <- getWord16be
  dp <- getWord16be
  sq <- getWord32be
  ack <- getWord32be
  return $ TCP sp dp sq ack
