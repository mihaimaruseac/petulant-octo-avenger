import Diagrams.Backend.CmdLine (mainRender)

import Data.Time (getCurrentTime, utctDay)

import Args
import Demos
import TSSMess
import Wars

main :: IO ()
main = do
  args <- parseArgs
  case args of
    -- demos first
    Demo n o     -> mainRender o $ selectDemo (Tutorial n)
    ADemo n o    -> mainRender o $ selectDemo (Arrow' n)
    TDemo n o    -> mainRender o $ selectDemo (Trails n)
    VDemo n o    -> mainRender o $ selectDemo (Vector n)
    -- special demos
    Arrow o      -> mainRender o demoArrow
    Tournament o -> mainRender o demoTournament
    -- other
    TSSMess      -> tssMess
    Wars o       -> doWars o
    _            -> print args

doWars :: DO -> IO ()
doWars o = do
  today <- fmap utctDay getCurrentTime
  mainRender o $ wars today
