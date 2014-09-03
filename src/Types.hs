{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Word
import Network.Pcap

import qualified Data.ByteString as B

import TCP

type ChanneledHeaderRequest = (HTTPRequestType, URI, [RequestHeader], RequestPayload, [ResponseHeader], ResponsePayload)
type ChanneledRequest = (HTTPRequestType, URI, RequestPayload, RequestPayload, ResponsePayload, ResponsePayload)
type CookedPacket = (PktHdr, Payload)
type HTTPTaggedRequest = (HTTPRequestType, RequestPayload, ResponsePayload)
type HTTPURIRequest = (HTTPRequestType, URI, RequestPayload, ResponsePayload)
type Header = (HeaderType, HeaderValue)
type HeaderType = Payload
type HeaderValue = Payload
type LinkLength = Int
type Payload = B.ByteString
type Port = Word16
type Request = (RequestPayload, ResponsePayload)
type RequestHeader = Header
type RequestPayload = Payload
type ResponseHeader = Header
type ResponsePayload = Payload
type SeqNo = Word32
type TCPC = (TCPConversationState, TCPConversation)
type TCPConversation = [TCPPayload]
type TCPPayload = (TCP, Payload)
type URI = Payload

data TCPConversationState = Ongoing | CloseFin | CloseFinACK | CloseACK
  deriving (Eq, Show, Ord, Enum)

data HTTPRequestType = GET | POST
  deriving (Eq, Show, Ord, Enum)

searchHeader :: Payload -> [Header] -> Payload
searchHeader h hs = maybe "" id $ lookup h hs
