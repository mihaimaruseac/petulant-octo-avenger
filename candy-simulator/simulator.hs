{-# LANGUAGE RecordWildCards #-}

{-
 - Simulate the amount of action points (APs) one gets after consuming several
 - tons (the unit of measure) of candy (popular name for one commodity) in the
 - game of Pardus (http://pardus.at), based on the amount of sugar already in
 - his blood and the value of trip-control/meditation skill (TC).
 -}

import Control.Monad.State
import Data.List
import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances

import Debug.Trace

-- number of hours of simulation
gInitialTime = 10000
-- min, max AP random gain
gMinAP = 200
gMaxAP = 250
-- diabetes factor
gFactor = 8

data Action
  = Wait Int
  | Take Int
  deriving (Eq, Show)

data Player a = P
  { g :: a
  , plan :: [Action]
  , tripControl :: Int
  , timeLeft :: Int
  , sugarFactor :: Int
  , tonsTaken :: Int
  , apsGained :: Int
  }

instance Show (Player a) where
  show P{..} = concat [ show plan, " ", show tripControl, " ", show tonsTaken, " ", show apsGained]

makeAgent :: RandomGen g => g -> [Action] -> Int -> Player g
makeAgent g p tc = P g p tc gInitialTime 0 0 0

stepAgent :: RandomGen g => State (Player g) ()
stepAgent = do
  p <- get
  if timeLeft p < 0 then return () else doStepAgent

doStepAgent :: RandomGen g => State (Player g) ()
doStepAgent = do
  p@P{..} <- get
  put $ trace (show timeLeft) $ p {timeLeft = timeLeft - 10000}
  stepAgent

generateAPs :: RandomGen g => Int -> Int -> Int -> g -> (Int, g)
generateAPs tc sg cd g = (appt * cd - gFactor * s, g')
  where
    (appt, g') = randomR (gMinAP, gMaxAP) g
    s = sum [sg .. sg + cd - 1]
