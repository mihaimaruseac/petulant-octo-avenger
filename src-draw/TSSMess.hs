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
  | VeteranFightered
  | Doctored
  | MisguidedVigilanted
  | Gunned
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

newtype Cooldown = Cooldown Int deriving (Show, Eq)

data RandomRoleRange
  = NotFrom [Role]
  | Replace [(Role, [Role])]
  deriving (Show, Eq)

data RoleInfo
  = Role Role Int
  | RandomRole Int RandomRoleRange
  deriving (Show, Eq)

data Mechanic
  = WinWhenDead [Role]
  -- particular mechanics
  | KillAtNight [Role] Consensus Count -- roles that can be killed if list is not empty
  | Communication Comm
  | AnonymousMessage Identified Count
  | Bomb BombPuzzle Count -- must match exactly those Int to blow
  | HackID
  | Lobby Consensus Count
  | Gun Cooldown
  | ConfiscateGun
  | DiesWhenKills Role
  | NightImmunity
  | ProtectAtNight Self
  | Doctors Role
  | Compromise [Role]
  | AttackerIdOnProtection [Role]
  | Know [RoleInfo]
  | ReadDropboxes
  deriving (Show, Eq)

data SpecialMechanic
  = Votes Vote
  | BloodlustOnTiedVotes
  | Suicide Int
  | ListOfTargettedPilots [Role] Int
  | DocGunKill
  | DocGunTSSRandomTSSKill
  | WildcardNotTargetted
  | DropBoxes
  | GroupLeaders
  | AllWildcards
  | RPCharacterStory
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
pAgile = P "Agile"
pBarackAlIssteg = P "Barack Al Issteg"
pBeep = P "Beep"
pBlah = P "Blah"
pBomb = P "Bomb"
pBstr = P "Bstr"
pCaledor = P "Caledor"
pCommandaguy = P "Commandaguy"
pCrackpot = P "Crackpot"
pDarsia = P "Darsia"
pDiablo = P "Diablo"
pElMalo = P "El Malo"
pEsmereldaWeatherwax = P "Esmerelda Weatherwax"
pFUrquhart = P "F Urquhart"
pFehera = P "Fehera"
pFenrir = P "Fenrir"
pFlink = P "Flink"
pGarkosTheDevourer = P "Garkos the Devourer"
pHamsterAlien = P "Hamster Alien"
pHatelove = P "Hatelove"
pHellequin = P "Hellequin"
pHolidayKoval = P "Holiday Koval"
pHorizon = P "Horizon"
pIrk = P "Irk"
pKennyYoobaStard = P "Kenny Yooba Stard"
pKillforfood = P "Killforfood"
pKurburis = P "Kurburis"
pLoyalty = P "Loyalty"
pMarcus = P "Marcus"
pMikillThomas = P "Mikill Thomas"
pMilkyway = P "Milkyway"
pNanuq = P "Nanuq"
pNashSteelfist = P "Nash Steelfist"
pNeight = P "Neight"
pNolt = P "Nolt"
pProle = P "Prole"
pRedKomodo = P "Red Komodo"
pShine = P "Shine"
pSkyCrossbones  = P "Sky Crossbones"
pSolarGeo = P "Solar Geo"
pSonofWarson = P "Son of Warson"
pTarraEclipse = P "Tarra Eclipse"
pTheCloneRanger = P "The Clone Ranger"
pThePwnlyCollective = P "The Pwnly Collective"
pTheSheep = P "The Sheep"
pTudytudysavaki = P "Tudytudysavaki"
pTyMercer = P "Ty Mercer"
pVegas = P "Vegas"
pWesR = P "Wes R"
pXolarix = P "Xolarix"
pXorism = P "Xorism"

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
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Neutral, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Retry $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Retry $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Retry $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [TSS, MisguidedVigilante])
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage OwnID $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS])
  , (IllegalDealer, Lobby WConsensus $ PerDay $ I 1)
  , (IllegalDealer, WinWhenDead [TSS, EPS])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, DiesWhenKills Neutral)
  , (VeteranFighter, WinWhenDead [TSS])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfAllowed)
  , (Doctor, WinWhenDead [TSS])
  , (MisguidedVigilante, KillAtNight [] WOConsensus $ PerGame Used $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Votes Public, BloodlustOnTiedVotes, Suicide 2]

g2 :: Game
g2 = G pXolarix (fromGregorian 2011 9 28)
  (fromGregorian 2011 10 8) (fromGregorian 2011 10 16)
  "Lies and Deceit" "Please kill me in a hilarious fashion"
  [ (pAgile, Wildcard Hacker, VeteranFightered)
  , (pBarackAlIssteg, TSS, Lynched)
  , (pBomb, TSS, Lynched)
  , (pCommandaguy, Empire, Survived)
  , (pDarsia, Union, Bombed)
  , (pDiablo, Empire, TSSed)
  , (pFUrquhart, Empire, Survived)
  , (pFehera, Federation, Survived)
  , (pFenrir, Union, Lynched)
  , (pHamsterAlien, Federation, Survived)
  , (pHellequin, Federation, TSSed)
  , (pHolidayKoval, TSS, MisguidedVigilanted)
  , (pHorizon, Neutral, Won)
  , (pKennyYoobaStard, Neutral, Won)
  , (pKillforfood, Wildcard IllegalDealer, Suicided)
  , (pKurburis, Doctor, Won)
  , (pLoyalty, IllegalDealer, Doctored)
  , (pMarcus, Neutral, TSSed)
  , (pMikillThomas, Union, Bombed)
  , (pMilkyway, Hacker, TSSed)
  , (pNanuq, Union, Lynched)
  , (pNashSteelfist, Wildcard EPS, Lynched)
  , (pNeight, Neutral, Won)
  , (pProle, Empire, Survived)
  , (pRedKomodo, VeteranFighter, Won)
  , (pSkyCrossbones, TSS, Lynched)
  , (pSolarGeo, MisguidedVigilante, Lynched)
  , (pTarraEclipse, Federation, TSSed)
  , (pThePwnlyCollective, TSS, MisguidedVigilanted)
  , (pTheSheep, EPS, Won)
  , (pTyMercer, Neutral, Won)
  , (pWesR, TSS, VeteranFightered)
  , (pXorism, Neutral, Won)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Empire, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Union, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [TSS, MisguidedVigilante])
  , (Neutral, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Neutral, Lobby WConsensus $ PerDay $ I 1)
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS, VeteranFighter])
  , (IllegalDealer, Gun $ Cooldown 2)
  , (IllegalDealer, WinWhenDead [TSS, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 2)
  , (VeteranFighter, DiesWhenKills Neutral)
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (EPS, NightImmunity)
  , (EPS, ConfiscateGun)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfAllowed)
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, Compromise [Hacker, VeteranFighter])
  , (Doctor, Doctors IllegalDealer)
  , (Doctor, AttackerIdOnProtection [TSS, MisguidedVigilante])
  , (MisguidedVigilante, KillAtNight [Neutral, TSS] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Votes Public, BloodlustOnTiedVotes, Suicide 2
  , ListOfTargettedPilots [Doctor, IllegalDealer] 5
  , DocGunKill, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g3 :: Game
g3 = G pMikillThomas (fromGregorian 2011 10 16)
  (fromGregorian 2011 10 24) (fromGregorian 2011 10 28)
  "Directions and Misdirections" "If you make yourself invincible I'll laugh at you in your death scene"
  [ (pBarackAlIssteg, Federation, Bombed)
  , (pBlah, EPS, DiedAtEndOfGame)
  , (pBstr, Union, VeteranFightered)
  , (pElMalo, Union, Bombed)
  , (pFUrquhart, Doctor, DiedAtEndOfGame)
  , (pFehera, Neutral, TSSed)
  , (pFenrir, Neutral, Won)
  , (pFlink, Neutral, Won)
  , (pGarkosTheDevourer, MisguidedVigilante, Lynched)
  , (pIrk, Federation, Bombed)
  , (pMarcus, TSS, Gunned)
  , (pMilkyway, Hacker, DiedAtEndOfGame)
  , (pRedKomodo, TSS, MisguidedVigilanted)
  , (pShine, TSS, DiedAtEndOfGame)
  , (pSkyCrossbones, EPS, DiedAtEndOfGame)
  , (pSolarGeo, Empire, Won)
  , (pSonofWarson, VeteranFighter, TSSed)
  , (pThePwnlyCollective, Empire, Won)
  , (pTheSheep, Neutral, MisguidedVigilanted)
  , (pTudytudysavaki, TSS, Lynched)
  , (pTyMercer, Doctor, DiedAtEndOfGame)
  , (pWesR, IllegalDealer, DiedAtEndOfGame)
  , (pXolarix, TSS, Lynched)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication DeadLetterDrop)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Federation, Communication DeadLetterDrop)
  , (Empire, Communication DeadLetterDrop)
  , (Union, Communication DeadLetterDrop)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Empire, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Union, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [TSS, MisguidedVigilante])
  , (Neutral, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Neutral, Know [Role Neutral 2, RandomRole 1 $ Replace [(TSS, [Doctor,
      Hacker, IllegalDealer, MisguidedVigilante, VeteranFighter])]])
  , (Neutral, Lobby WConsensus $ PerDay $ I 1)
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS, VeteranFighter])
  , (Hacker, ReadDropboxes)
  , (Hacker, Know [RandomRole 2 $ NotFrom [EPS, VeteranFighter]])
  , (IllegalDealer, Gun $ Cooldown 2)
  , (IllegalDealer, WinWhenDead [TSS, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 2)
  , (VeteranFighter, DiesWhenKills Neutral)
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (VeteranFighter, Know [RandomRole 1 $ Replace [(TSS, [Doctor, IllegalDealer,
      MisguidedVigilante, Neutral])]])
  , (EPS, NightImmunity)
  , (EPS, ConfiscateGun)
  , (EPS, WinWhenDead [TSS, Hacker, IllegalDealer])
  , (EPS, Know [RandomRole 2 $ NotFrom [TSS]])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, Compromise [Hacker, VeteranFighter])
  , (Doctor, Doctors IllegalDealer)
  , (Doctor, AttackerIdOnProtection [TSS, MisguidedVigilante])
  , (Doctor, Know [RandomRole 2 $ NotFrom [Hacker, Doctor]])
  , (MisguidedVigilante, KillAtNight [Neutral, TSS] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  , (MisguidedVigilante, Know [RandomRole 2 $ NotFrom [MisguidedVigilante, Neutral]])
  ]
  [DropBoxes, AllWildcards, RPCharacterStory, Suicide 2, GroupLeaders
  , Votes Public, BloodlustOnTiedVotes, Suicide 2
  , DocGunKill, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g4 :: Game
g4 = G pFenrir (fromGregorian 2011 10 29)
  (fromGregorian 2011 11 5) (fromGregorian 2011 11 10)
  "Insanity is Fun" "We all want to unleash our inner sadist in thinking up ways to kill"
  [ (pBarackAlIssteg, EPS, Lynched)
  , (pBlah, Union, VeteranFightered)
  , (pBstr, TSS, Lynched)
  , (pCaledor, Empire, TSSed)
  , (pCrackpot, TSS, Won)
  , (pEsmereldaWeatherwax, MisguidedVigilante, Gunned)
  , (pFUrquhart, Neutral, TSSed)
  , (pFlink, Federation, TSSed)
  , (pIrk, Union, TSSed)
  , (pMarcus, IllegalDealer, Doctored)
  , (pMikillThomas, Neutral, Won)
  , (pMilkyway, TSS, Won)
  , (pRedKomodo, VeteranFighter, Lynched)
  , (pShine, EPS, Lynched)
  , (pSolarGeo, TSS, Won)
  , (pSonofWarson, Empire, Suicided)
  , (pThePwnlyCollective, Hacker, Lynched)
  , (pTudytudysavaki, Federation, Bombed)
  , (pWesR, Doctor, TSSed)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication DeadLetterDrop)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Federation, Communication DeadLetterDrop)
  , (Empire, Communication DeadLetterDrop)
  , (Union, Communication DeadLetterDrop)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Empire, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Union, Bomb NumberOfMatches $ PerGame Retry $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [TSS, MisguidedVigilante])
  , (Neutral, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (Neutral, Know [Role Neutral 2, RandomRole 1 $ Replace [(TSS, [Doctor,
      Hacker, IllegalDealer, MisguidedVigilante, VeteranFighter])]])
  , (Neutral, Lobby WConsensus $ PerDay $ I 1)
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS, VeteranFighter])
  , (Hacker, ReadDropboxes)
  , (Hacker, Know [RandomRole 2 $ NotFrom [EPS, VeteranFighter]])
  , (IllegalDealer, Gun $ Cooldown 2)
  , (IllegalDealer, WinWhenDead [Federation, Empire, Union, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 2)
  , (VeteranFighter, DiesWhenKills Neutral)
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (VeteranFighter, Know [RandomRole 1 $ Replace [(TSS, [Doctor, IllegalDealer,
      MisguidedVigilante, Neutral])]])
  , (EPS, NightImmunity)
  , (EPS, ConfiscateGun)
  , (EPS, WinWhenDead [TSS, Hacker, IllegalDealer])
  , (EPS, Know [RandomRole 2 $ NotFrom [TSS]])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, Compromise [Hacker, VeteranFighter])
  , (Doctor, Doctors IllegalDealer)
  , (Doctor, AttackerIdOnProtection [TSS, MisguidedVigilante])
  , (Doctor, Know [RandomRole 2 $ NotFrom [Hacker, Doctor]])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  , (MisguidedVigilante, Know [RandomRole 2 $ NotFrom [MisguidedVigilante, Neutral]])
  ]
  [DropBoxes, AllWildcards, RPCharacterStory, Suicide 2, GroupLeaders
  , Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

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
