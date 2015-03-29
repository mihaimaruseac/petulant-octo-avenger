import Diagrams.Backend.CmdLine (mainRender)

import Args
import Demos

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Demo n o     -> mainRender o $ selectDemo (Tutorial n)
    ADemo n o    -> mainRender o $ selectDemo (Arrow' n)
    TDemo n o    -> mainRender o $ selectDemo (Trails n)
    VDemo n o    -> mainRender o $ selectDemo (Vector n)
    Arrow o      -> mainRender o $ demoArrow
    Tournament o -> mainRender o $ demoTournament
    _            -> print args
