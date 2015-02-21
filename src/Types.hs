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
type ASP = Int
type ATP = Double
type CompetencyLevel = Int
type CompetencyPercentage = Int
type Credits = Int
type FactionLevel = Payload -- TODO: change to Int
type FactionPercentage = Int
type PlayersOnline = Int
type RepF = Int
type RepE = Int
type RepU = Int
type RepA = Int
type XP = Int

data DBCommand
  = AP AP
  | ASP ASP
  | ATP ATP
  | Competency CompetencyLevel CompetencyPercentage
  | Credits Credits
  | Debug Payload
  | Faction FactionLevel FactionPercentage
  | MM [Tag Payload]
  | POnline PlayersOnline
  | Rep RepF RepE RepU RepA
  | Turnover Credits
  | XP XP
  deriving Show
