import Diagrams.Backend.CmdLine (mainRender)

import Args
import Demos

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Demo n o     -> mainRender o $ selectDemo (Tutorial n)
    TDemo n o    -> mainRender o $ selectDemo (Trails n)
    VDemo n o    -> mainRender o $ selectDemo (Vector n)
    Tournament o -> mainRender o $ demoTournament
    Arrow o      -> mainRender o $ demoArrow
    _            -> print args
