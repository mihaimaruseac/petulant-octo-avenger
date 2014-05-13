module Types where

import Network.Pcap

import qualified Data.ByteString as B

type LinkLength = Int
type Payload = B.ByteString
type CookedPacket = (PktHdr, B.ByteString)
type ProcessPacket = Payload -> Payload
