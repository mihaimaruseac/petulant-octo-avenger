{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TSSMess (tssMess) where

import Control.Lens
import Data.Text
import Data.Time

tssMess = undefined

{- types -}

newtype Player = P Text deriving Show

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
  deriving (Show, Eq)

data Status
  = Suicided -- inactivity
  | Lynched
  | TSSed
  | Bombed
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
data Count = PerGame RetryOrUsed Int | PerDay Int deriving (Show, Eq)
data Self = SelfAllowed | SelfDenied deriving (Show, Eq)

data Mechanic
  = Kill Role
  | KillOr [Role]
  -- particular mechanics
  | KillAtNight Consensus Count
  | Communication Comm
  | AnonymousMessage Identified Count
  | Bomb Int Count -- must match exactly those Int to blow
  | HackID
  | Lobby
  | DiesWhenKills Role
  | NightImmunity
  | ProtectAtNight Self
  deriving (Show, Eq)

data SpecialMechanic
  = Votes Vote
  | BloodlustOnTiedVotes
  | Suicide Int
  deriving (Show, Eq)

{-
 -}

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

{- players -}
pBeep = P "Beep"
pBomb = P "Bomb"
pCommandaguy = P "Commandaguy"
pDiablo = P "Diablo"
pGarkosTheDevourer = P "Garkos the Devourer"
pHamsterAlien = P "Hamster Alien"
pHatelove = P "Hatelove"
pKillforfood = P "Killforfood"
pMarcus = P "Marcus"
pMilkyway = P "Milkyway"
pNanuq = P "Nanuq"
pNolt = P "Nolt"
pProle = P "Prole"
pRedKomodo = P "Red Komodo"
pSolarGeo = P "Solar Geo"
pTheCloneRanger = P "The Clone Ranger"
pThePwnlyCollective = P "The Pwnly Collective"
pTheSheep = P "The Sheep"
pVegas = P "Vegas"
pWesR = P "Wes R"
pXolarix = P "Xolarix"

{- games -}
g1 :: Game
g1 = G pXolarix (fromGregorian 2011 9 18)
  (fromGregorian 2011 9 22) (fromGregorian 2011 9 30)
  "Stalemate" "You'll nebah tek me alibe"
  [ (pBeep, TSS, Suicided)
  , (pBomb, MisguidedVigilante, Lynched)
  , (pCommandaguy, Empire, TSSed)
  , (pDiablo, TSS, Lynched)
  , (pGarkosTheDevourer, Doctor, Won)
  , (pHamsterAlien, EPS, Lynched)
  , (pHatelove, Neutral, TSSed)
  , (pKillforfood, Empire, Bombed)
  , (pMarcus, Union, TSSed)
  , (pMilkyway, Union, Won)
  , (pNanuq, TSS, Lynched)
  , (pNolt, IllegalDealer, Lynched)
  , (pProle, TSS, Lynched)
  , (pRedKomodo, Federation, Lynched)
  , (pSolarGeo, VeteranFighter, TSSed)
  , (pTheCloneRanger, Hacker, TSSed)
  , (pThePwnlyCollective, Neutral, TSSed)
  , (pTheSheep, Neutral, TSSed)
  , (pVegas, Federation, Lynched)
  , (pWesR, Neutral, Won)
  ]
  [ (TSS, KillAtNight WConsensus $ PerDay 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay 1)
  , (TSS, Kill Federation)
  , (TSS, Kill Empire)
  , (TSS, Kill Union)
  , (TSS, Kill Neutral)
  , (TSS, Kill Hacker)
  , (TSS, Kill IllegalDealer)
  , (TSS, Kill VeteranFighter)
  , (TSS, Kill EPS)
  , (TSS, Kill Doctor)
  , (TSS, Kill MisguidedVigilante)
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, Kill TSS)
  , (Federation, Kill Empire)
  , (Federation, Kill Union)
  , (Empire, Kill TSS)
  , (Empire, Kill Federation)
  , (Empire, Kill Union)
  , (Union, Kill TSS)
  , (Union, Kill Federation)
  , (Union, Kill Empire)
  , (Federation, Bomb 2 $ PerGame Retry 2)
  , (Empire, Bomb 2 $ PerGame Retry 2)
  , (Union, Bomb 2 $ PerGame Retry 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, Kill TSS)
  , (Neutral, Kill MisguidedVigilante)
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage OwnID $ PerDay 1)
  , (IllegalDealer, Lobby)
  , (IllegalDealer, Kill TSS)
  , (IllegalDealer, Kill EPS)
  , (VeteranFighter, KillAtNight WOConsensus $ PerGame Used 1)
  , (VeteranFighter, DiesWhenKills Neutral)
  , (VeteranFighter, Kill TSS)
  , (EPS, NightImmunity)
  , (EPS, Kill TSS)
  , (EPS, KillOr [Hacker, IllegalDealer])
  , (Doctor, ProtectAtNight SelfAllowed)
  , (Doctor, Kill TSS)
  , (MisguidedVigilante, KillAtNight WOConsensus $ PerGame Used 4)
  , (MisguidedVigilante, Kill Neutral)
  , (MisguidedVigilante, Kill TSS)
  ]
  [Votes Public, BloodlustOnTiedVotes, Suicide 2]

--

{-
 - Games:
 - http://forum.pardus.at/archive/index.php?showtopic=55254&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=55372&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=55622&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=55850&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=56043&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=60311&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=60552&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=60684&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=60837&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=62666&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=62795&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=62905&st=0
 - http://forum.pardus.at/archive/index.php?showtopic=65885&st=0
 -
 - Notes:
 - http://forum.pardus.at/archive/index.php?showtopic=55254&st=15&#entry1112281 This is not a combat game
 - http://forum.pardus.at/archive/index.php?showtopic=55372&st=105&#entry1118753 We have maps
 - http://forum.pardus.at/archive/index.php?showtopic=55372&st=360&#entry1121896 Some numbers
 - http://forum.pardus.at/archive/index.php?showtopic=55622&st=60&#entry1123559 Poetry
 - http://forum.pardus.at/archive/index.php?showtopic=55622&st=135&#entry1125240 Security seal or too much geekery?
 - http://forum.pardus.at/archive/index.php?showtopic=55850&st=120&#entry1129702 Survival? No, we have hormones
 - http://forum.pardus.at/archive/index.php?showtopic=60552&st=105&#entry1225650 Bloody kills
 - http://forum.pardus.at/archive/index.php?showtopic=60552&st=210&#entry1227035 Getting screwed and screwdrivers..
 - http://forum.pardus.at/archive/index.php?showtopic=60684&st=120&#entry1229656 ..or wrenches
 -}
