module Wars.Types where

import Data.Time

data Event = E
  { _startDate :: Day
  , _endDate :: Day
  , _type :: EventType
  }

data EventType
  = Peace
  | War -- TODO: add details here or as an extra field above?
  | AllianceConflict
