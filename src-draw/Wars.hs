module Wars (wars) where

import Data.Time

import Wars.Types

wars = print events

events :: [Event]
events =
  [ E artemisOpened vendettaStart Peace
  ]
  where
    artemisOpened = fromGregorian 2007 6 10
    vendettaStart = fromGregorian 2008 8 8
