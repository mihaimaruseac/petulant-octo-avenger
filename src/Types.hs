module Types where

import Text.HTML.TagSoup (Tag)

import qualified Data.ByteString as B

{-- Types for parsing the wire content and transforming it to HTML. --}
type ChanneledHeaderRequest = (HTTPRequestType, URI, [RequestHeader], RequestPayload, [ResponseHeader], ResponsePayload)
type Header = (HeaderType, HeaderValue)
type HeaderType = Payload
type HeaderValue = Payload
type Payload = B.ByteString
type RequestHeader = Header
type RequestPayload = Payload
type ResponseHeader = Header
type ResponsePayload = Payload
type TaggedHeaderRequest = (HTTPRequestType, URI, [RequestHeader], RequestPayload, [ResponseHeader], [Tag Payload])
type URI = Payload

data HTTPRequestType = GET | POST
  deriving (Eq, Show, Ord, Enum)

{-- Types for getting the data out of HTML and into the DB. --}
type PlayersOnline = Int
type CompetencyLevel = Int

data DBCommand
  = POnline PlayersOnline
  | PFstats CompetencyLevel
  | Debug Payload
  | MM [Tag Payload]
  deriving Show

data TaggedInfo
  = Fail ChanneledHeaderRequest
  | OK DBCommand

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr
  show (OK dbc) = show dbc
