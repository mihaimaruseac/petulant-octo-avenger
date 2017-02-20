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
type NPCKill = Int
type NPCName = Payload
type PlayersOnline = Int
type RepA = Int
type RepE = Int
type RepF = Int
type RepU = Int
type Skill = Double
type XP = Int

data DBCommand
  = AP AP
  | AM Skill Skill
  | ASP ASP
  | ATP ATP
  | AllianceID Int
  | AllianceMemberCount Int
  | AllianceMember Payload
  | Bounties Bounties
  | CLK Skill Skill
  | Competency CompetencyLevel CompetencyPercentage
  | Credits Credits
  | Debug Payload
  | DestroyBounties Bounties
  | EC Skill Skill
  | ENG Skill Skill
  | FC Skill Skill
  | Faction FactionLevel FactionPercentage
  | GC Skill Skill
  | HA Skill Skill
  | HACK Skill Skill
  | KillBounties Bounties
  | MAN Skill Skill
  | MM [Tag Payload]
  | NPCKill KDCount
  | NPCList [(NPCName, NPCKill)]
  | POnline PlayersOnline
  | PilotDeath KDCount
  | PilotKill KDCount
  | Rep RepF RepE RepU RepA
  | Ribbons Medals
  | TAC Skill Skill
  | Turnover Credits
  | WEAP Skill Skill
  | WarMedals Medals
  | XP XP
  deriving Show
