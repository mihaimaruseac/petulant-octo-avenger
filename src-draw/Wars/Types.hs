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
  | War { _winner :: Faction
        , _loser :: Faction
        , _war_details :: (WarDetails, WarDetails)
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
  deriving Show

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
