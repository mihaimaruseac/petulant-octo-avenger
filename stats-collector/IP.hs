{-
 - Parse IP header conforming to http://tools.ietf.org/html/rfc791
 -}
module IP (IP(..), skipIP) where

import Data.Bits
import Data.Serialize
import Data.Word

import Types

data IP = IP
  { version :: Version
  , hlen :: Int
  , tos :: Word8
  } deriving Show

data Version = IPv4 | IPv6 deriving Show

skipIP :: ProcessPacket
skipIP payload = print $ runGetPartial parseIP payload

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
  tos' <- getWord8
  return $ IP IPv4 len tos'

parseIPv6 :: Int -> Get IP
parseIPv6 = error $ "Parsing IPv6 is not implemented yet."
