module Wars (wars) where

import Control.Lens
import Data.Time

import Wars.Types

wars = print events

events :: [Event]
events =
  [ peace & startDate .~ artemisOpened & endDate ?~ vendettaStart
  , war & details .~ vendetta & name ?~ "Vendetta War"
        & startDate .~ vendettaStart & endDate ?~ vendettaEnd
  , peace & startDate .~ vendettaEnd & endDate ?~ dominionIStart
  , war & details .~ dominionI & name ?~ "Dominion I War"
        & startDate .~ dominionIStart & endDate ?~ dominionIEnd
  , peace & startDate .~ dominionIEnd & endDate ?~ dominionIIStart & name ?~ "hateful"
  , war & details .~ dominionII & name ?~ "Dominion II War"
        & startDate .~ dominionIIStart & endDate ?~ dominionIIEnd
  , peace & startDate .~ dominionIIEnd & endDate ?~ prosperityStart & name ?~ "bountyful"
  , war & details .~ prosperity & name ?~ "Prosperity War"
        & startDate .~ prosperityStart & endDate ?~ prosperityEnd
  , peace & startDate .~ prosperityEnd & endDate ?~ lastWarStart & name ?~ "privateer disturbed"
  , war & details .~ lastWar & name ?~ "The Last War"
        & startDate .~ lastWarStart & endDate ?~ lastWarEnd
  , peace & startDate .~ lastWarEnd & endDate ?~ warsonGate & name ?~ "deceitful"
  , majorEvent & startDate .~ warsonGate & name ?~ "Warsongate"
  , peace & startDate .~ warsonGate & endDate ?~ splitStart & name ?~ "Treaty Enforced"
  , localConflict & startDate .~ splitStart & endDate ?~ splitEnd & name ?~ "Split Conflict (Feds Failed Taking Veedfa... Again)"
  , peace & startDate .~ splitEnd & name ?~ "a strange"
  ]
  where
    artemisOpened   = fromGregorian 2007  6 10
    vendettaStart   = fromGregorian 2008  8  8
    vendettaEnd     = fromGregorian 2008  9 17
    dominionIStart  = fromGregorian 2009  1 18
    dominionIEnd    = fromGregorian 2009  3  4
    dominionIIStart = fromGregorian 2009 12 25
    dominionIIEnd   = fromGregorian 2010  2 12
    prosperityStart = fromGregorian 2010 12 27
    prosperityEnd   = fromGregorian 2011  2 19
    lastWarStart    = fromGregorian 2012  7 15
    lastWarEnd      = fromGregorian 2012  9 10
    warsonGate      = fromGregorian 2013  4  9
    splitStart      = fromGregorian 2014  9 11
    splitEnd        = fromGregorian 2015  1  3
    vendetta   = War Federation Empire (vendettaFed, vendettaEmp)
    dominionI  = War Union Federation (dominionIUni, dominionIFed)
    dominionII = War Federation Union (dominionIIFed, dominionIIUni)
    prosperity = War Federation Empire (prosperityFed, prosperityEmp)
    lastWar    = War Federation Union (lastWarFed, lastWarUni)
    vendettaFed   = Details 315944 185653  15591 114700      0 243 389
    vendettaEmp   = Details 230784 180219  13165  37400      0 210 322
    dominionIUni  = Details 626901 345115  28586 188200  65000 223 463
    dominionIFed  = Details 253492 239480  12812   1200      0 193 306
    dominionIIUni = Details 463370 353590  33180 106600      0 173 373
    dominionIIFed = Details 518503 344271  57432  76800  40000 222 443
    prosperityFed = Details 895138 441423 119915  53800 280000 247 579
    prosperityEmp = Details 390711 311371  39740  39600      0 193 368
    lastWarUni    = Details 480084 207297  46187 226600      0 153 356
    lastWarFed    = Details 495682 265319  72163  78200  80000 178 377