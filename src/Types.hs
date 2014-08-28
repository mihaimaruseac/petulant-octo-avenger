{-# LANGUAGE OverloadedStrings #-}

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

type HeaderType = Payload
type HeaderValue = Payload
type Header = (HeaderType, HeaderValue)

type RequestPayload = Payload
type RequestHeader = Header
type ResponsePayload = Payload
type ResponseHeader = Header
type URI = Payload

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)

data HTTPRequestType = GET | POST
  deriving (Eq, Show, Ord, Enum)

searchHeader :: Payload -> [Header] -> Payload
searchHeader h hs = maybe "" id $ lookup h hs
