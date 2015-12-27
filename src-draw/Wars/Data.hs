module Wars.Data (events) where

import Control.Lens hiding ((#))
import Data.Time

import Wars.Types

events, artemis, orion, pegasus :: [Event]
events = head [artemis, orion, pegasus]

artemis =
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
  , peace & startDate .~ warsonGate & endDate ?~ splitStart & name ?~ "treaty enforced"
  , localConflict & startDate .~ splitStart & endDate ?~ splitEnd & name ?~ "Split Conflict (Feds Failed Taking Veedfa... Again)"
  , peace & startDate .~ splitEnd & name ?~ "a strange"-- & endDate ?~ mithakenStart
  -- , war & details .~ mithaken & name ?~ "Mithaken"
  --       & startDate .~ mithakenStart & endDate ?~ mithakenEnd
  -- , war & details .~ esb & name ?~ "Empire Strikes Back"
  --       & startDate .~ esbStart & endDate ?~ esbEnd
  , war & details .~ galactic & name ?~ "Second Galactic"
        & startDate .~ mithakenStart & endDate ?~ esbEnd
  , peace & startDate .~ esbEnd & name ?~ "rebuilding"
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
    mithakenStart   = fromGregorian 2015  9 12
    --mithakenEnd     = fromGregorian 2015 11 11
    --esbStart        = fromGregorian 2015 10  6
    esbEnd          = fromGregorian 2015 11 14
    vendetta   = War Federation Empire (vendettaFed, vendettaEmp)
    dominionI  = War Union Federation (dominionIUni, dominionIFed)
    dominionII = War Federation Union (dominionIIFed, dominionIIUni)
    prosperity = War Federation Empire (prosperityFed, prosperityEmp)
    lastWar    = War Federation Union (lastWarFed, lastWarUni)
    --mithaken   = War Federation Union (mithakenFed, mithakenUni)
    --esb        = War Empire Federation (esbEmp, esbFed)
    galactic   = War Empion Federation (galacticEmpion, galacticFed)
    vendettaFed   = Details 315944 185653  15591 114700      0 243 389
    vendettaEmp   = Details 230784 180219  13165  37400      0 210 322
    dominionIUni  = Details 626901 345115  28586 188200  65000 223 463
    dominionIFed  = Details 253492 239480  12812   1200      0 193 306
    dominionIIUni = Details 463370 323590  33180 106600      0 173 373
    dominionIIFed = Details 518503 344271  57432  76800  40000 222 443
    prosperityFed = Details 895138 441423 119915  53800 280000 247 579
    prosperityEmp = Details 390711 311371  39740  39600      0 193 368
    lastWarUni    = Details 480084 207297  46187 226600      0 153 356
    lastWarFed    = Details 495682 265319  72163  78200  80000 178 377
    --mithakenFed   = Details 338727  94258  56869 157600  30000 261 108
    --mithakenUni   = Details 317808  89251  44557 184000      0 197  78
    --esbEmp        = Details 268741 105752  60189  67800  35000 195  90
    --esbFed        = Details 221953  78997  42256 100700      0 174  78
    galacticEmpion= Details 586549 195003 104746 251800  35000 392 302
    galacticFed   = Details 560680 173255  99125 258300  30000 435 270
-- end artemis

orion =
  [ peace & startDate .~ orionOpened & endDate ?~ usubeStart
  , war & details .~ usube & name ?~ "Usube War"
        & startDate .~ usubeStart & endDate ?~ usubeEnd
  , peace & startDate .~ usubeEnd & endDate ?~ separatistStart
  , war & details .~ separatist & name ?~ "Separatist War"
        & startDate .~ separatistStart & endDate ?~ separatistEnd
  , peace & startDate .~ separatistEnd & endDate ?~ phageStart
  , war & details .~ phage & name ?~ "Phage War"
        & startDate .~ phageStart & endDate ?~ phageEnd
  , peace & startDate .~ phageEnd & endDate ?~ discordStart
  , war & details .~ discord & name ?~ "Discord War"
        & startDate .~ discordStart & endDate ?~ discordEnd
  , peace & startDate .~ discordEnd & endDate ?~ imperialistStart
  , war & details .~ imperialist & name ?~ "Imperialist War"
        & startDate .~ imperialistStart & endDate ?~ imperialistEnd
  , peace & startDate .~ imperialistEnd & endDate ?~ restorationStart
  , war & details .~ restoration & name ?~ "Restoration War"
        & startDate .~ restorationStart & endDate ?~ restorationEnd
  , peace & startDate .~ restorationEnd & endDate ?~ dagobertStart
  , war & details .~ dagobert & name ?~ "Dagobert's War (Triple Ten War)"
        & startDate .~ dagobertStart & endDate ?~ dagobertEnd
  , peace & startDate .~ dagobertEnd & endDate ?~ dawnStart
  , war & details .~ dawn & name ?~ "Red Dawn War"
        & startDate .~ dawnStart & endDate ?~ dawnEnd
  , peace & startDate .~ dawnEnd & endDate ?~ onizukaStart
  , war & details .~ onizuka & name ?~ "Onizuka's War"
        & startDate .~ onizukaStart & endDate ?~ onizukaEnd
  , peace & startDate .~ onizukaEnd
  ]
  where
    orionOpened      = fromGregorian 2004  9 14
    usubeStart       = fromGregorian 2007  2  2
    usubeEnd         = fromGregorian 2007  4 21
    separatistStart  = fromGregorian 2007 11 11
    separatistEnd    = fromGregorian 2007 12 18
    phageStart       = fromGregorian 2008  5 11
    phageEnd         = fromGregorian 2008  6 27
    discordStart     = fromGregorian 2009  3 21
    discordEnd       = fromGregorian 2009  4 27
    imperialistStart = fromGregorian 2009  7 10
    imperialistEnd   = fromGregorian 2009  8 27
    restorationStart = fromGregorian 2009 12 26
    restorationEnd   = fromGregorian 2010  2 16
    dagobertStart    = fromGregorian 2010 10 10
    dagobertEnd      = fromGregorian 2010 11 29
    dawnStart        = fromGregorian 2011 12  9
    dawnEnd          = fromGregorian 2012  1 19
    onizukaStart     = fromGregorian 2013  8 17
    onizukaEnd       = fromGregorian 2013 10 15
    usube       = War Empire Federation (usubeEmp, usubeFed)
    separatist  = War Empire Union (separatistEmp, separatistUni)
    phage       = War Empire Federation (phageEmp, phageFed)
    discord     = War Union Federation (discordUni, discordFed)
    imperialist = War Empire Union (imperialistEmp, imperialistUni)
    restoration = War Empire Federation (restorationEmp, restorationFed)
    dagobert    = War Empire Union (dagobertEmp, dagobertUni)
    dawn        = War Empire Union (dawnEmp, dawnUni)
    onizuka     = War Empire Union (onizukaEmp, onizukaUni)
    usubeFed       = Details 517383 408445  27038   6900  75000 294 171
    usubeEmp       = Details 596503 558093  21610   1800  15000 287 205
    separatistEmp  = Details 424835 383674  30361  10800      0 272 463
    separatistUni  = Details 338493 241677  34916   6900  55000 293 443
    phageFed       = Details 562521 320045  38776 203700      0 341 589
    phageEmp       = Details 578812 388537  49175 106100  35000 292 545
    discordUni     = Details 570247 397594  36653  21000 115000 244 475
    discordFed     = Details 374807 224910  31297 118600      0 246 416
    imperialistEmp = Details 565113 374873  52840  47400  90000 226 445
    imperialistUni = Details 365337 303423  38114  23800      0 183 352
    restorationFed = Details 536978 268925  48553 219500      0 218 445
    restorationEmp = Details 655145 308944  64801 111400 170000 234 480
    dagobertEmp    = Details 643799 315334  45365 283100      0 200 451
    dagobertUni    = Details 464190 318894  68096  62200  15000 175 370
    dawnEmp        = Details 484389 209742  49747 224900      0 148 366
    dawnUni        = Details 243074 153116  12358  77600      0 115 219
    onizukaEmp     = Details 924800 233233 125167 416400 150000 166 475
    onizukaUni     = Details 373851 147540  26211 200100      0 105 241
-- end orion

pegasus =
  [ peace & startDate .~ pegasusOpened & endDate ?~ shimokitaStart
  , war & details .~ shimokita & name ?~ "Shimokita Anomaly"
        & startDate .~ shimokitaStart & endDate ?~ shimokitaEnd
  , peace & startDate .~ shimokitaEnd
  ]
  where
    pegasusOpened  = fromGregorian 2007  6 10
    shimokitaStart = fromGregorian 2015  2 26
    shimokitaEnd   = fromGregorian 2015  4  7
    shimokita = War Empire Union (shimokitaEmp, shimokitaFed)
    shimokitaEmp = Details 399436 31257 19279 308900 40000 43 160
    shimokitaFed = Details 153982 19985  2397 131600     0 30  78
-- end pegasus
