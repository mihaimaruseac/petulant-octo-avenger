{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Word
import Network.Pcap

import qualified Data.ByteString as B

import TCP

type LinkLength = Int
type Payload = B.ByteString

type CookedPacket = (PktHdr, Payload)
type TCPConversation = [TCPPayload]
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

type TCPPayload = (TCP, Payload)

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)

data HTTPRequestType = GET | POST
  deriving (Eq, Show, Ord, Enum)

searchHeader :: Payload -> [Header] -> Payload
searchHeader h hs = maybe "" id $ lookup h hs
