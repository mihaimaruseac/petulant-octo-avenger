module Wars (wars) where

import Control.Lens
import Data.Time

import Wars.Types

wars = print events

events :: [Event]
events =
  [ mkPeace artemisOpened vendettaStart
  , War   (Duration vendettaStart $ Just vendettaEnd) undefined undefined
  ]
  where
    artemisOpened = fromGregorian 2007 6 10
    vendettaStart = fromGregorian 2008 8 8
    vendettaEnd   = fromGregorian 2008 9 17
