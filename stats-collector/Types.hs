module Types where

import Data.Word
import Network.Pcap

import qualified Data.ByteString as B

import TCP

type LinkLength = Int
type Payload = B.ByteString
type CookedPacket = (PktHdr, B.ByteString)
type TCPConversation = [(TCP, Payload)]
type Port = Word16
type SeqNo = Word32

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)
