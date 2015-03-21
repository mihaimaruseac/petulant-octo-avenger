{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.CmdLine (mainRender)

import Args
import Demos

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Demo n o     -> mainRender o $ selectDemo (Tutorial n)
    VDemo n o    -> mainRender o $ selectDemo (Vector n)
    Tournament o -> mainRender o $ demoTournament
    _            -> print args
