module Types where

import qualified Data.ByteString as B

type LinkLength = Int
type Payload = B.ByteString
type ProcessPacket = Payload -> IO ()
