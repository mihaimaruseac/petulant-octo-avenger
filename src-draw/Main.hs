import Diagrams.Backend.CmdLine (mainRender)

import Args
import Demos
import TSSMess
import WarActivity
import Wars

main :: IO ()
main = do
  args <- parseArgs
  case args of
    -- demos first
    Demo n o         -> mainRender o $ selectDemo (Tutorial n)
    ADemo n o        -> mainRender o $ selectDemo (Arrow' n)
    TDemo n o        -> mainRender o $ selectDemo (Trails n)
    VDemo n o        -> mainRender o $ selectDemo (Vector n)
    -- special demos
    Arrow o          -> mainRender o demoArrow
    Tournament o     -> mainRender o demoTournament
    -- other
    TSSMess m o      -> doTssMess m (mainRender o)
    Wars o           -> doWars (mainRender o)
    Activity d off o -> doWA d off (mainRender o)
    -- default catch
    _            -> print args
