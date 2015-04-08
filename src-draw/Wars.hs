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
  ]
  where
    artemisOpened   = fromGregorian 2007 6 10
    vendettaStart   = fromGregorian 2008 8 8
    vendettaEnd     = fromGregorian 2008 9 17
    vendetta = War Federation Empire (vendettaFed, vendettaEmp)
    vendettaFed = Details 315944 185653 15591 114700 0 243 389
    vendettaEmp = Details 230784 180219 13165  37400 0 210 322
