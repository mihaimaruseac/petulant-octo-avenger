{-# LANGUAGE TemplateHaskell #-}

module TSSMess.Types where

import Control.Lens (makeLenses, (^.))
import Data.Text (Text)
import Data.Time (Day)

newtype Player = P Text deriving (Show, Eq)

data Role
  = TSS
  | Federation
  | Empire
  | Union
  | Neutral
  | Hacker
  | IllegalDealer
  | VeteranFighter
  | EPS
  | Doctor
  | MisguidedVigilante
  | Wildcard Role
  -- wildcard specific roles
  | NeutralProtector -- Xolarix2013
  | Lobbyist --Marcus2013
  | Sniper --Marcus2013
  | Infected --Marcus2013
  | MercenaryAssassin --Dino2013
  | EPSCrussader --Dino2014
  deriving (Show, Eq)

data Status
  = Suicided -- inactivity
  | Lynched
  | TSSed
  | Bombed
  | VeteranFightered
  | Doctored
  | MisguidedVigilanted
  | Gunned
  | RNGed
  | Sniperred
  | Assasinated
  -- the following are end-game conditions
  | DiedAtEndOfGame
  | Survived
  | Won
  deriving (Show, Eq)

data Vote = Public | Private deriving (Show, Eq)
data Consensus = WConsensus | WOConsensus deriving (Show, Eq)
data Comm = Freely | WithinGroup | DeadLetterDrop deriving (Show, Eq)
data Identified = CompletelyAnonymous | HackerID | OwnID deriving (Show, Eq)
data RetryOrUsed = Retry | Used deriving (Show, Eq)
data CountVal = I Int | EqRole [Role] deriving (Show, Eq)
data Count = PerGame RetryOrUsed CountVal | PerDay CountVal deriving (Show, Eq)
data Self = SelfAllowed | SelfDenied deriving (Show, Eq)
data BombPuzzle = ExactMatch | NumberOfMatches deriving (Show, Eq)
data Immune = Immune | NoImmune deriving (Show, Eq)
data Overdose = Overdose | NoOverdose deriving (Show, Eq)

newtype Cooldown = Cooldown Int deriving (Show, Eq)

data RandomRoleRange
  = NotFrom [Role]
  | Replace [(Role, [Role])]
  deriving (Show, Eq)

data RoleInfo
  = Role Role Int
  | RandomRole Int RandomRoleRange
  deriving (Show, Eq)

data TrophyAction
  = Bloodlust
  | Impersonation
  | Firewall Cooldown
  | Execution
  deriving (Show, Eq)

data Mechanic
  = WinWhenDead [Role]
  -- particular mechanics
  | KillAtNight [Role] Consensus Count -- roles that can be killed if list is not empty
  | Communication Comm
  | AnonymousMessage Identified Count
  | Bomb BombPuzzle Count -- must match exactly those Int to blow
  | HackID
  | HackIDTwice Cooldown -- 50% of being locked down Cooldown
  | RevealWildcardMechanic
  | Lobby Consensus Count
  | Lobby2 Consensus Int Cooldown
  | Gun Cooldown
  | ConfiscateGun
  | DiesWhenKills [Role]
  | NightImmunity
  | ProtectAtNight Self
  | Doctors Role
  | Compromise [Role]
  | CompromiseWounded
  | AttackerIdOnProtection [Role]
  | Know [RoleInfo]
  | ReadDropboxes
  | TrophyPoints [TrophyAction]
  | Paralyze Immune Overdose Count
  | Arrest
  | Wound Immune Cooldown
  | BlockOnFailure Cooldown
  | NoTSSKillWhenWon
  deriving (Show, Eq)

data SpecialMechanic
  = Votes Vote
  | BloodlustOnTiedVotes
  | Suicide Int
  | ListOfTargettedPilots [Role] Int
  | DocGunKill -- kill protected target if Doc has gun
  | DocGunKillSelf -- kill doc if protected target has gun
  | DocGunTSSRandomTSSKill -- kill TSS if protected target is attacked by tss and has gun
  | DocIDBotHDie -- doc and ID both die if doc protects ID
  | WildcardNotTargetted
  | DropBoxes
  | GroupLeaders
  | AllWildcards
  | RPCharacterStory
  | RNGKills
  | TurnTSS
  | RedShirts [Player]
  deriving (Show, Eq)

data Game = G
  { _dm :: Player
  , _announcementDate :: Day
  , _startDate :: Day
  , _endDate :: Day
  , _title :: Text
  , _quote :: Text
  , _players :: [(Player, Role, Status)]
  , _mechanics :: [(Role, Mechanic)]
  , _specialMechanics :: [SpecialMechanic]
  } deriving (Show)
makeLenses ''Game

instance Eq Game where
  g1 == g2 = g1 ^. announcementDate == g2 ^. announcementDate
