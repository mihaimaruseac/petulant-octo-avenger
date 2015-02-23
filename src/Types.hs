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
type Bounties = Int
type CompetencyLevel = Int
type CompetencyPercentage = Int
type Credits = Int
type FactionLevel = Payload -- TODO: change to Int
type FactionPercentage = Int
type KDCount = Int
type Medals = Int
type PlayersOnline = Int
type RepA = Int
type RepE = Int
type RepF = Int
type RepU = Int
type XP = Int

data DBCommand
  = AP AP
  | ASP ASP
  | ATP ATP
  | Bounties Bounties
  | Competency CompetencyLevel CompetencyPercentage
  | Credits Credits
  | Debug Payload
  | DestroyBounties Bounties
  | Faction FactionLevel FactionPercentage
  | KillBounties Bounties
  | MM [Tag Payload]
  | NPCKill KDCount
  | POnline PlayersOnline
  | PilotDeath KDCount
  | PilotKill KDCount
  | Rep RepF RepE RepU RepA
  | Ribbons Medals
  | Turnover Credits
  | WarMedals Medals
  | XP XP
  deriving Show
