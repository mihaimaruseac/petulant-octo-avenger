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
type AP = Int
type CompetencyLevel = Int
type CompetencyPercentage = Int
type FactionLevel = Payload -- TODO: change to Int
type FactionPercentage = Int
type PlayersOnline = Int

data DBCommand
  = AP AP
  | Competency CompetencyLevel CompetencyPercentage
  | Faction FactionLevel FactionPercentage
  | POnline PlayersOnline
  | Debug Payload
  | MM [Tag Payload]
  deriving Show
