{-
 - Simulate the amount of action points (APs) one gets after consuming several
 - tons (the unit of measure) of candy (popular name for one commodity) in the
 - game of Pardus (http://pardus.at), based on the amount of sugar already in
 - his blood and the value of trip-control/meditation skill (TC).
 -}

import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances

type TripControl = Int
type SugarInSystem = Int
type Candy = Int
type AP = Int

generateAPs :: RandomGen g => TripControl -> SugarInSystem -> Candy -> g -> (AP, g)
generateAPs tc sg cd g = (appt * cd - 8 * s, g')
  where
    (appt, g') = randomR (200, 250) g
    s = sum [sg .. sg + cd - 1]
