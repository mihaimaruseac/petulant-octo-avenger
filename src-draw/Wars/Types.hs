{-# LANGUAGE TemplateHaskell #-}

module Wars.Types where

import Control.Lens
import Data.Default
import Data.Time

data Event = Event
  { _name :: Maybe String
  , _startDate :: Day
  , _endDate :: Maybe Day
  , _details :: EventDetails
  } deriving (Show)

data EventDetails
  = Peace
  | War { _factions :: (Faction, Faction)
        , _winner :: Faction
        , _war_details :: (WarDetails, WarDetails)
        }
  | LocalConflict
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
  deriving Show

makeLenses ''Event
makeLenses ''EventDetails
makeLenses ''WarDetails

instance Default Event where
  def = Event Nothing (fromGregorian 0 0 0) Nothing Peace

peace :: Event
peace = def

mkPeace :: Day -> Day -> Event
mkPeace st en = undefined --Peace $ Duration st $ Just en

mkPeace' :: Day -> Event
mkPeace' st = undefined --Peace $ Duration st Nothing

mkWar :: Day -> Day -> (Faction, Faction) -> Faction -> (WarDetails, WarDetails) -> Event
mkWar st en = undefined --War (Duration st $ Just en)

mkWar' :: Day -> (Faction, Faction) -> Faction -> (WarDetails, WarDetails) -> Event
mkWar' st = undefined --War (Duration st Nothing)
