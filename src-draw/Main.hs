{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.CmdLine

import Args
import Demos

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Demo n o -> mainRender o $ selectDemo n
    Tournament o -> mainRender o $ demoTournament
    _ -> print args
