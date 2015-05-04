{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Wars.Types where

import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time

data Event = Event
  { _name :: Maybe String
  , _startDate :: Day
  , _endDate :: Maybe Day
  , _details :: EventDetails
  } deriving (Show)

data EventDetails
  = Peace
  | War { _winner :: Faction
        , _loser :: Faction
        , _warDetails :: (WarDetails, WarDetails)
        }
  | LocalConflict
  | MajorEvent
  deriving (Show)

data WarDetails = Details
  { _points :: Int
  , _kills :: Int
  , _structures :: Int
  , _mission :: Int
  , _sector :: Int
  , _heroes :: Int
  , _medals :: Int
  } deriving Show

data Faction
  = Federation
  | Empire
  | Union
  deriving (Show, Eq)

-- NOTE that this is not transitive!
instance Ord Faction where
  Federation <= Empire = True
  Empire <= Union = True
  Union <= Federation = True
  x <= y = x == y

instance Monoid WarDetails where
  mempty = Details 0 0 0 0 0 0 0
  -- TODO: really fugly
  (Details p1 k1 s1 m1 sc1 h1 md1) `mappend` (Details p2 k2 s2 m2 sc2 h2 md2)
    = Details (max p1 p2) (max k1 k2) (max s1 s2) (max m1 m2) (max sc1 sc2) (max h1 h2) (max md1 md2)

makeLenses ''Event
makeLenses ''EventDetails
makeLenses ''WarDetails

instance Default Event where
  def = Event Nothing (fromGregorian 0 0 0) Nothing Peace

peace :: Event
peace = def

localConflict :: Event
localConflict = def & details .~ LocalConflict

war :: Event
war = def

majorEvent :: Event
majorEvent = def & details .~ MajorEvent

isPeace :: Event -> Bool
isPeace (view details -> Peace) = True
isPeace _ = False

isWar :: Event -> Bool
isWar (view details -> War {}) = True
isWar _ = False

isLocalConflict :: Event -> Bool
isLocalConflict (view details -> LocalConflict) = True
isLocalConflict _ = False

isMajorEvent :: Event -> Bool
isMajorEvent (view details -> MajorEvent) = True
isMajorEvent _ = False
