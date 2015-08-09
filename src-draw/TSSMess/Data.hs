{-# LANGUAGE OverloadedStrings #-}

module TSSMess.Data (games) where

import Data.Time (fromGregorian)

import TSSMess.Types

games :: [Game]
games = artemisGames

artemisGames :: [Game]
artemisGames = [g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12]

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
  , (VeteranFighter, DiesWhenKills [Neutral])
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
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
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
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (VeteranFighter, Know [RandomRole 2 $ Replace [(TSS, [Doctor, IllegalDealer,
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
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
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
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (VeteranFighter, Know [RandomRole 2 $ Replace [(TSS, [Doctor, IllegalDealer,
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
  [DropBoxes, RPCharacterStory, Suicide 2, GroupLeaders
  , Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g5 :: Game
g5 = G pMarcus (fromGregorian 2011 11 11)
  (fromGregorian 2012 1 17) (fromGregorian 2012 1 27)
  "Hidden Operative - Codenames" "You wet your pants but are still alive. For now."
  [ (pArose, Union, Bombed)
  , (pBarackAlIssteg, Wildcard Doctor, RNGed)
  , (pBlah, Neutral, RNGed)
  , (pDdaz, MisguidedVigilante, Lynched)
  , (pEsmereldaWeatherwax, EPS, Lynched)
  , (pFUrquhart, Empire, VeteranFightered)
  , (pFenrir, Hacker, VeteranFightered)
  , (pFlink, IllegalDealer, RNGed)
  , (pIrk, Union, Lynched)
  , (pMikillThomas, Neutral, RNGed)
  , (pMilkyway, Doctor, Lynched)
  , (pRedKomodo, VeteranFighter, Lynched)
  , (pShine, Wildcard Hacker, Won)
  , (pSolarGeo, Empire, Bombed)
  , (pThePwnlyCollective, Federation, Bombed)
  , (pTradeMachine, Federation, RNGed)
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
  , (VeteranFighter, WinWhenDead [TSS, Hacker])
  , (VeteranFighter, Know [RandomRole 2 $ Replace [(TSS, [Doctor, IllegalDealer,
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
  [DropBoxes, RPCharacterStory, Suicide 2, GroupLeaders
  , Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
  , RNGKills, DocIDBotHDie, TurnTSS
  ]

g6 :: Game
g6 = G pXolarix (fromGregorian 2012 10 27)
  (fromGregorian 2012 11 14) (fromGregorian 2012 11 21)
  "Save Gina" "Wonder why he died? He ran into space with no suit."
  [ (pCovington, EPS, Lynched)
  , (pDanteLongshadow, Neutral, TSSed)
  , (pDinonumber, TSS, Lynched)
  , (pEddieRikes, Neutral, MisguidedVigilanted)
  , (pElGringoBandito, VeteranFighter, DiedAtEndOfGame)
  , (pFUrquhart, IllegalDealer, TSSed)
  , (pGarkostheButcher, TSS, Lynched)
  , (pHuckleberry, Union, DiedAtEndOfGame)
  , (pMarcus, Wildcard MisguidedVigilante, Won)
  , (pPelor, MisguidedVigilante, Won)
  , (pReez, Empire, DiedAtEndOfGame)
  , (pRichert, Neutral, Lynched)
  , (pSream, TSS, Lynched)
  , (pTacoguy, Federation, DiedAtEndOfGame)
  , (pTarraEclipse, Empire, DiedAtEndOfGame)
  , (pTheCloneRanger, Federation, TSSed)
  , (pTheInsaneOne, Union, TSSed)
  , (pTro, Doctor, DiedAtEndOfGame)
  , (pUristMclovin, Hacker, Won)
  , (pVegas, Neutral, MisguidedVigilanted)
  , (pWildGina, TSS, Lynched)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 1)
  , (IllegalDealer, WinWhenDead [TSS, EPS])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, DiesWhenKills [Neutral])
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, WinWhenDead [TSS, VeteranFighter])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Used $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Public, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g7 :: Game
g7 = G pMarcus (fromGregorian 2012 11 21)
  (fromGregorian 2012 11 26) (fromGregorian 2012 12 4)
  "Ghosts and Bears" "Never fear for Tro is here with a wrench"
  [ (pAsriel, IllegalDealer, TSSed)
  , (pDanteLongshadow, TSS, VeteranFightered)
  , (pDinonumber, Hacker, TSSed)
  , (pEddieRikes, MisguidedVigilante, Suicided)
  , (pFUrquhart, TSS, Survived)
  , (pGarkostheButcher, Union, TSSed)
  , (pLauraDumitrescu, Union, Lynched)
  , (pMephistoles, Neutral, Won)
  , (pMilkyway, Neutral, Lynched)
  , (pPelor, Neutral, TSSed)
  , (pReez, EPS, Lynched)
  , (pSalathr, Empire, TSSed)
  , (pSream, TSS, Lynched)
  , (pSupercooli, TSS, Lynched)
  , (pTacoguy, Neutral, Won)
  , (pTheCloneRanger, Federation, TSSed)
  , (pTheInsaneOne, Federation, TSSed)
  , (pTro, VeteranFighter, Lynched)
  , (pUristMclovin, Doctor, Lynched)
  , (pWildGina, Empire, TSSed)
  , (pXolarix, Wildcard IllegalDealer, TSSed)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Neutral, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, Lobby WOConsensus $ PerGame Used $ I 1)
  , (Neutral, Know [Role Neutral 1])
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 1)
  , (IllegalDealer, WinWhenDead [TSS, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, DiesWhenKills [Neutral])
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, Paralyze NoImmune NoOverdose $ PerGame Used $ I 4)
  , (Doctor, WinWhenDead [TSS, VeteranFighter, MisguidedVigilante])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Used $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Public, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g8 :: Game
g8 = G pDinonumber (fromGregorian 2012 12 3)
  (fromGregorian 2012 12 12) (fromGregorian 2012 12 18)
  "EPS Fiasco" "If you start praying in Latin again I'll make your death the worst I can think of"
  [ (pAquilaSicarius, TSS, Suicided)
  , (pBlackChocolate, VeteranFighter, VeteranFightered)
  , (pDanteLongshadow, Neutral, Suicided)
  , (pFUrquhart, Wildcard Neutral, TSSed)
  , (pLauraDumitrescu, Union, TSSed)
  , (pMarcus, Empire, Bombed)
  , (pMephistoles, Empire, Survived)
  , (pMilkyway, MisguidedVigilante, TSSed)
  , (pPelor, Federation, Survived)
  , (pRedKomodo, Doctor, TSSed)
  , (pSalathr, Neutral, Survived)
  , (pSolarGeo, Neutral, Suicided)
  , (pSream, IllegalDealer, TSSed)
  , (pTheCloneRanger, Neutral, VeteranFightered)
  , (pTheInsaneOne, Hacker, Won)
  , (pThePwnlyCollective, Federation, Bombed)
  , (pThraxis, TSS, Lynched)
  , (pTro, EPS, Lynched)
  , (pVegas, Union, Lynched)
  , (pWildGina, TSS, Suicided)
  , (pXolarix, TSS, Lynched)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, Lobby WOConsensus $ PerGame Used $ I 1)
  , (Hacker, HackID)
  , (Hacker, HackIDTwice $ Cooldown 2)
  , (Hacker, AnonymousMessage CompletelyAnonymous $ PerDay $ I 1)
  , (Hacker, WinWhenDead [Federation, TSS, EPS, VeteranFighter])
  , (Hacker, WinWhenDead [Empire, TSS, EPS, VeteranFighter])
  , (Hacker, WinWhenDead [Union, TSS, EPS, VeteranFighter])
  , (IllegalDealer, Gun $ Cooldown 1)
  , (IllegalDealer, WinWhenDead [TSS, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, DiesWhenKills [Neutral])
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, Arrest)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, Paralyze Immune Overdose $ PerGame Used $ I 3)
  , (Doctor, DiesWhenKills [EPS, Neutral])
  , (Doctor, WinWhenDead [TSS, VeteranFighter, MisguidedVigilante])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Used $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill, WildcardNotTargetted
  ]

g9 :: Game
g9 = G pXolarix (fromGregorian 2013 5 30)
  (fromGregorian 2013 6 9) (fromGregorian 2013 6 14)
  "Gruesome Storm" "This is the only reason I'm not playing Skyrim now"
  [ (pAnger, Neutral, TSSed)
  , (pDinonumber, EPS, Won)
  , (pFUrquhart, Wildcard NeutralProtector, TSSed)
  , (pGamerguy, TSS, Suicided)
  , (pGarkostheButcher, Doctor, Lynched)
  , (pHugolum, Hacker, TSSed)
  , (pJoshuaCalvert, Federation, TSSed)
  , (pKylie, TSS, VeteranFightered)
  , (pMarcus, Empire, Won)
  , (pNashSteelfist, Union, TSSed)
  , (pObsequey, Neutral, Survived)
  , (pPiggieWiggie, TSS, Lynched)
  , (pReez, MisguidedVigilante, Survived)
  , (pSarthker, VeteranFighter, Won)
  , (pSaturnine, IllegalDealer, Survived)
  , (pSeneka, Empire, Won)
  , (pSextusPompeius, Union, Bombed)
  , (pSysice, Neutral, Lynched)
  , (pTarraEclipse, TSS, Lynched)
  , (pThraxis, Federation, Bombed)
  , (pTro, Neutral, Survived)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust, Impersonation, Firewall $ Cooldown 3])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage OwnID $ PerDay $ I 1)
  , (Hacker, WinWhenDead [Neutral, TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 1)
  , (IllegalDealer, WinWhenDead [Federation, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Empire, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Union, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, Wound Immune $ Cooldown 2)
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, CompromiseWounded)
  , (Doctor, WinWhenDead [TSS, VeteranFighter])
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, BlockOnFailure $ Cooldown 1)
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Public, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill
  ]

g10 :: Game
g10 = G pMarcus (fromGregorian 2013 6 15)
  (fromGregorian 2013 6 17) (fromGregorian 2013 6 25)
  "Whodunnit?" "We were killed the same night. I had a companion to Heaven."
  [ (pAnger, EPS, Lynched)
  , (pDanteLongshadow, Wildcard Infected, TSSed)
  , (pDinonumber, Doctor, TSSed)
  , (pGarkostheButcher, TSS, Lynched)
  , (pGrafEisen, Hacker, TSSed)
  , (pHugolum, Union, Suicided)
  , (pKurrai, TSS, Suicided)
  , (pKylie, Wildcard Sniper, Won)
  , (pLauraTheLovedOne, Federation, TSSed)
  , (pMiloStark, Union, Suicided)
  , (pPiggieWiggie, VeteranFighter, Suicided)
  , (pReez, TSS, Lynched)
  , (pSarthker, Neutral, Suicided)
  , (pSeneka, Wildcard Lobbyist, TSSed)
  , (pSextusPompeius, Neutral, Lynched)
  , (pSream, Federation, TSSed)
  , (pSysice, TSS, Lynched)
  , (pTarraEclipse, Neutral, Lynched)
  , (pTatmaraNholl, MisguidedVigilante, Sniperred)
  , (pThraxis, Neutral, Won)
  , (pTro, Empire, Sniperred)
  , (pWilliamUrquhart, IllegalDealer, TSSed)
  , (pXolarix, Empire, TSSed)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust, Impersonation, Firewall $ Cooldown 3])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, Lobby2 WConsensus 2 $ Cooldown 1)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage OwnID $ PerDay $ I 1)
  , (Hacker, WinWhenDead [Neutral, TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 0)
  , (IllegalDealer, WinWhenDead [Federation, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Empire, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Union, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, Wound Immune $ Cooldown 2)
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, CompromiseWounded)
  , (Doctor, WinWhenDead [TSS, VeteranFighter])
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Public, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill
  ]

g11 :: Game
g11 = G pDinonumber (fromGregorian 2013 6 29)
  (fromGregorian 2013 8 15) (fromGregorian 2013 8 23)
  "Mammoth" "I'll make your deaths painful, brutal and absolute"
  [ (pAnger, Federation, TSSed)
  , (pSextusPompeius, TSS, Won)
  , (pSeneka, VeteranFighter, TSSed)
  , (pKylie, Union, Suicided)
  , (pDiablo, Neutral, Suicided)
  , (pTEldor, Federation, Won)
  , (pSream, Neutral, Lynched)
  , (pGrafEisen, TSS, Lynched)
  , (pDodge, Empire, TSSed)
  , (pCalimond, Neutral, Suicided)
  , (pSmedley, TSS, Suicided)
  , (pThraxis, MisguidedVigilante, Suicided)
  , (pTro, IllegalDealer, TSSed)
  , (pBrenettoftheRills, Doctor, TSSed)
  , (pSalveCrossbones, Hacker, TSSed)
  , (pTarraEclipse, Wildcard MercenaryAssassin, TSSed)
  , (pPiggieWiggie, TSS, Lynched)
  , (pAnthonya, Union, Assasinated)
  , (pCurfin, Neutral, Lynched)
  , (pReez, EPS, TSSed)
  , (pLornanRoche, Empire, Suicided)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, Lobby2 WConsensus 2 $ Cooldown 1)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, NoTSSKillWhenWon)
  , (Hacker, HackID)
  , (Hacker, AnonymousMessage OwnID $ PerDay $ I 1)
  , (Hacker, WinWhenDead [Neutral, TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 0)
  , (IllegalDealer, WinWhenDead [Federation, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Empire, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Union, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, Wound Immune $ Cooldown 2)
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, CompromiseWounded)
  , (Doctor, WinWhenDead [TSS, VeteranFighter])
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill
  , RedShirts [pInvictio, pFUrquhart, pLauraTheLovedOne, pSysice, pWildGina,
    pXolarix, pYarok, pGarkostheButcher, pSarthker, pDemonswrath, pThanu,
    pGeePig, pMicase]
  ]

g12 :: Game
g12 = G pDinonumber (fromGregorian 2014 8 26)
  (fromGregorian 2014 9 13) (fromGregorian 2013 9 19)
  "Flares of Polaris" "I expect to be severin' his bones soon"
  [ (pCarimo, EPS, Won)
  , (pChadDeoxy, Neutral, Survived)
  , (pEddieBLanner, Federation, Suicided)
  , (pFera, MisguidedVigilante, Survived)
  , (pHerneTheHunter, Neutral, MisguidedVigilanted)
  , (pLauraTheLovedOne, VeteranFighter, TSSed)
  , (pMattGray, Neutral, MisguidedVigilanted)
  , (pMikillThomas, IllegalDealer, Suicided)
  , (pMikkas, Neutral, Suicided)
  , (pMissSmokey, Federation, Bombed)
  , (pMistyMoonlight, TSS, Suicided)
  , (pMith, Empire, TSSed)
  , (pPelor, Wildcard EPSCrussader, TSSed)
  , (pSenty, Union, TSSed)
  , (pSeverin, Union, Lynched)
  , (pSirius, TSS, Lynched)
  , (pStarflight, TSS, Lynched)
  , (pTheInsaneOne, Doctor, Won)
  , (pTro, Empire, Lynched)
  , (pVegas, Hacker, Suicided)
  , (pXolarix, TSS, Lynched)
  ]
  [ (TSS, KillAtNight [] WConsensus $ PerDay $ I 1)
  , (TSS, Communication WithinGroup)
  , (TSS, AnonymousMessage OwnID $ PerDay $ I 1)
  , (TSS, WinWhenDead [Federation, Empire, Union, Hacker,
      IllegalDealer, VeteranFighter, EPS, Doctor, MisguidedVigilante])
  , (TSS, TrophyPoints [Bloodlust, Execution])
  , (Federation, Communication WithinGroup)
  , (Empire, Communication WithinGroup)
  , (Union, Communication WithinGroup)
  , (Federation, WinWhenDead [TSS, Empire, Union])
  , (Empire, WinWhenDead [TSS, Federation, Union])
  , (Union, WinWhenDead [TSS, Federation, Empire])
  , (Federation, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Empire, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Union, Bomb ExactMatch $ PerGame Used $ I 2)
  , (Neutral, Communication DeadLetterDrop)
  , (Neutral, Lobby2 WConsensus 2 $ Cooldown 2)
  , (Neutral, WinWhenDead [Federation, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Empire, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, WinWhenDead [Union, VeteranFighter, Hacker, MisguidedVigilante])
  , (Neutral, NoTSSKillWhenWon)
  , (Hacker, HackID)
  , (Hacker, RevealWildcardMechanic)
  , (Hacker, AnonymousMessage OwnID $ PerDay $ I 1)
  , (Hacker, WinWhenDead [Neutral, TSS, EPS])
  , (IllegalDealer, Gun $ Cooldown 0)
  , (IllegalDealer, WinWhenDead [Federation, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Empire, EPS, Doctor])
  , (IllegalDealer, WinWhenDead [Union, EPS, Doctor])
  , (VeteranFighter, KillAtNight [] WOConsensus $ PerGame Used $ I 1)
  , (VeteranFighter, Wound Immune $ Cooldown 2)
  , (VeteranFighter, WinWhenDead [TSS, MisguidedVigilante])
  , (VeteranFighter, WinWhenDead [TSS, Doctor])
  , (EPS, NightImmunity)
  , (EPS, WinWhenDead [TSS, Hacker])
  , (EPS, WinWhenDead [TSS, IllegalDealer])
  , (Doctor, ProtectAtNight SelfDenied)
  , (Doctor, CompromiseWounded)
  , (Doctor, WinWhenDead [TSS, VeteranFighter])
  , (Doctor, WinWhenDead [TSS, IllegalDealer])
  , (MisguidedVigilante, KillAtNight [Neutral] WOConsensus $ PerGame Retry $ EqRole [Neutral])
  , (MisguidedVigilante, WinWhenDead [Neutral, TSS])
  ]
  [Suicide 2, Votes Private, BloodlustOnTiedVotes
  , DocGunKill, DocGunKillSelf, DocGunTSSRandomTSSKill
  ]

{- players -}
pAgile, pAnger, pAnthonya, pAquilaSicarius, pArose, pAsriel, pBarackAlIssteg, pBeep, pBlackChocolate, pBlah, pBomb, pBrenettoftheRills, pBstr, pCaledor, pCalimond, pCarimo, pChadDeoxy, pCommandaguy, pCovington, pCrackpot, pCurfin, pDanteLongshadow, pDarsia, pDdaz, pDemonswrath, pDiablo, pDinonumber, pDodge, pEddieBLanner, pEddieRikes, pElGringoBandito, pElMalo, pEsmereldaWeatherwax, pFUrquhart, pFehera, pFenrir, pFera, pFlink, pGamerguy, pGarkosTheDevourer, pGarkostheButcher, pGeePig, pGrafEisen, pHamsterAlien, pHatelove, pHellequin, pHerneTheHunter, pHolidayKoval, pHorizon, pHuckleberry, pHugolum, pInvictio, pIrk, pJoshuaCalvert, pKennyYoobaStard, pKillforfood, pKurburis, pKurrai, pKylie, pLauraDumitrescu, pLauraTheLovedOne, pLornanRoche, pLoyalty, pMarcus, pMattGray, pMephistoles, pMicase, pMikillThomas, pMikkas, pMilkyway, pMiloStark, pMissSmokey, pMistyMoonlight, pMith, pNanuq, pNashSteelfist, pNeight, pNolt, pObsequey, pPelor, pPiggieWiggie, pProle, pRedKomodo, pReez, pRichert, pSalathr, pSalveCrossbones, pSarthker, pSaturnine, pSeneka, pSenty, pSeverin, pSextusPompeius, pShine, pSirius, pSkyCrossbones, pSmedley, pSolarGeo, pSonofWarson, pSream, pStarflight, pSupercooli, pSysice, pTEldor, pTacoguy, pTarraEclipse, pTatmaraNholl, pThanu, pTheCloneRanger, pTheInsaneOne, pThePwnlyCollective, pTheSheep, pThraxis, pTradeMachine, pTro, pTudytudysavaki, pTyMercer, pUristMclovin, pVegas, pWesR, pWildGina, pWilliamUrquhart, pXolarix, pXorism, pYarok :: Player
pAgile = P "Agile"
pAnger = P "Anger"
pAnthonya = P "Anthonya"
pAquilaSicarius = P "Aquila Sicarius"
pArose = P "Arose"
pAsriel = P "Asriel"
pBarackAlIssteg = P "Barack Al Issteg"
pBeep = P "Beep"
pBlackChocolate = P "Black Chocolate"
pBlah = P "Blah"
pBomb = P "Bomb"
pBrenettoftheRills = P "Brenett of the Rills"
pBstr = P "Bstr"
pCaledor = P "Caledor"
pCalimond = P "Calimond"
pCarimo = P "Carimo"
pChadDeoxy = P "Chad Deoxy"
pCommandaguy = P "Commandaguy"
pCovington = P "Covington"
pCrackpot = P "Crackpot"
pCurfin = P "Curfin"
pDanteLongshadow = P "Dante Longshadow"
pDarsia = P "Darsia"
pDdaz = P "Ddaz"
pDemonswrath = P "Demonswrath"
pDiablo = P "Diablo"
pDinonumber = P "Dinonumber"
pDodge = P "Dodge"
pEddieBLanner = P "Eddie B Lanner"
pEddieRikes = P "Eddie Rikes"
pElGringoBandito = P "El Gringo Bandito"
pElMalo = P "El Malo"
pEsmereldaWeatherwax = P "Esmerelda Weatherwax"
pFUrquhart = P "F Urquhart"
pFehera = P "Fehera"
pFenrir = P "Fenrir"
pFera = P "Fera"
pFlink = P "Flink"
pGamerguy = P "Gamerguy"
pGarkosTheDevourer = P "Garkos the Devourer"
pGarkostheButcher = P "Garkos the Butcher"
pGeePig = P "Gee Pig"
pGrafEisen = P "Graf Eisen"
pHamsterAlien = P "Hamster Alien"
pHatelove = P "Hatelove"
pHellequin = P "Hellequin"
pHerneTheHunter = P "Herne The Hunter"
pHolidayKoval = P "Holiday Koval"
pHorizon = P "Horizon"
pHuckleberry = P "Huckleberry"
pHugolum = P "Hugolum"
pInvictio = P "Invictoo"
pIrk = P "Irk"
pJoshuaCalvert = P "Joshua Calvert"
pKennyYoobaStard = P "Kenny Yooba Stard"
pKillforfood = P "Killforfood"
pKurburis = P "Kurburis"
pKurrai = P "Kurrai"
pKylie = P "Kylie"
pLauraDumitrescu = P "Laura Dumitrescu"
pLauraTheLovedOne = P "Laura The Loved One"
pLornanRoche = P "Lornan Roche"
pLoyalty = P "Loyalty"
pMarcus = P "Marcus"
pMattGray = P "Matt Gray"
pMephistoles = P "Mephistoles"
pMicase = P "Micase"
pMikillThomas = P "Mikill Thomas"
pMikkas = P "Mikkas"
pMilkyway = P "Milkyway"
pMiloStark = P "Milo Stark"
pMissSmokey = P "Miss Smokey"
pMistyMoonlight = P "Misty Moonlight"
pMith = P "Mith"
pNanuq = P "Nanuq"
pNashSteelfist = P "Nash Steelfist"
pNeight = P "Neight"
pNolt = P "Nolt"
pObsequey = P "Obsequey"
pPelor = P "Pelor"
pPiggieWiggie = P "Piggie Wiggie"
pProle = P "Prole"
pRedKomodo = P "Red Komodo"
pReez = P "Reez"
pRichert = P "Richert"
pSalathr = P "Salathr"
pSalveCrossbones = P "Salve Crossbones"
pSarthker = P "Sarthker"
pSaturnine = P "Saturnine"
pSeneka = P "Seneka"
pSenty = P "Senty"
pSeverin = P "Severin"
pSextusPompeius = P "Sextus Pompeius"
pShine = P "Shine"
pSirius = P "Sirius"
pSkyCrossbones  = P "Sky Crossbones"
pSmedley = P "Smedley"
pSolarGeo = P "Solar Geo"
pSonofWarson = P "Son of Warson"
pSream = P "Sream"
pStarflight = P "Starflight"
pSupercooli = P "Supercooli"
pSysice = P "Sysice"
pTEldor = P "T Eldor"
pTacoguy = P "Tacoguy"
pTarraEclipse = P "Tarra Eclipse"
pTatmaraNholl = P "Tatmara Nholl"
pThanu = P "Thanu"
pTheCloneRanger = P "The Clone Ranger"
pTheInsaneOne = P "The Insane One"
pThePwnlyCollective = P "The Pwnly Collective"
pTheSheep = P "The Sheep"
pThraxis = P "Thraxis"
pTradeMachine = P "Trade Machine"
pTro = P "Tro"
pTudytudysavaki = P "Tudytudysavaki"
pTyMercer = P "Ty Mercer"
pUristMclovin = P "Urist Mclovin"
pVegas = P "Vegas"
pWesR = P "Wes R"
pWildGina = P "Wild Gina"
pWilliamUrquhart = P "William Urquhart"
pXolarix = P "Xolarix"
pXorism = P "Xorism"
pYarok = P "Yarok"

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
 - Notes (as funny quotes):
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
