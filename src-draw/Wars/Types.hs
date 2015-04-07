{-# LANGUAGE TemplateHaskell #-}

module Wars.Types where

import Control.Lens
import Data.Time

data Event
  = Peace { _d :: Duration }
  | War { _d :: Duration
        , _factions :: (Faction, Faction)
        , _winner :: Faction
        , _details :: (WarDetails, WarDetails)
        }
  | LocalConflict { _d :: Duration}
  deriving (Show)

data Duration = Duration
  { _startDate :: Day
  , _endDate :: Maybe Day
  } deriving (Show)

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
  deriving Show

makeLenses ''Duration
makeLenses ''Event
makeLenses ''WarDetails

mkPeace :: Day -> Day -> Event
mkPeace st en = Peace $ Duration st $ Just en

mkPeace' :: Day -> Event
mkPeace' st = Peace $ Duration st Nothing

mkWar :: Day -> Day -> (Faction, Faction) -> Faction -> (WarDetails, WarDetails) -> Event
mkWar st en = War (Duration st $ Just en)

mkWar' :: Day -> (Faction, Faction) -> Faction -> (WarDetails, WarDetails) -> Event
mkWar' st = War (Duration st Nothing)
