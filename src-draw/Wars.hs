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
  ]
  where
    artemisOpened  = fromGregorian 2007 6 10
    vendettaStart  = fromGregorian 2008 8  8
    vendettaEnd    = fromGregorian 2008 9 17
    dominionIStart = fromGregorian 2009 1 18
    dominionIEnd   = fromGregorian 2009 3  4
    vendetta  = War Federation Empire (vendettaFed, vendettaEmp)
    dominionI = War Union Federation (dominionIUni, dominionIFed)
    vendettaFed  = Details 315944 185653 15591 114700     0 243 389
    vendettaEmp  = Details 230784 180219 13165  37400     0 210 322
    dominionIUni = Details 626901 345115 28586 188200 65000 223 463
    dominionIFed = Details 253492 239480 12812   1200     0 193 306
