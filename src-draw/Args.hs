module Args where

import Diagrams.Backend.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import System.Environment

import qualified Options.Applicative as O

type DO = (DiagramOpts, DiagramLoopOpts)

data Commands
  = Demo Int DO
  | Tournament DO
  | NoDiagram

instance Show Commands where
  show (Demo n (d, _)) = mconcat ["Demo ", show n, " ", show d]
  show (Tournament (d, _)) = mconcat ["Tournament ", show d]
  show NoDiagram = show "NoDiagram"

-- parser for all modes
parseModes :: O.Parser Commands
parseModes = O.subparser (O.command "demo" parseDemo O.<> O.metavar "demo")
       O.<|> O.subparser (O.command "tournament" parseTournament O.<> O.metavar "tournament")
       O.<|> O.subparser (O.command "nodia" parseNoDiagram O.<> O.metavar "nodia")

-- individual parsers
parseDemo :: O.ParserInfo Commands
parseDemo = flip O.info mod . (O.helper O.<*>) $ Demo
  O.<$> O.option O.auto
      (    O.short 'n'
      O.<> O.long "number"
      O.<> O.help "Demo number"
      O.<> O.metavar "INT"
      O.<> O.value 14
      O.<> O.showDefault
      -- O.<> completer (bashCompleter "smth") -- disabled because of not being implemented
      )
  O.<*> parser
  where
    mod = O.fullDesc O.<> O.header "Draw demo diagram from tutorial" O.<> O.footer "by MM"

parseTournament :: O.ParserInfo Commands
parseTournament = flip O.info mod . (O.helper O.<*>) $ Tournament O.<$> parser
  where
    mod = O.fullDesc O.<> O.header "Draw demo tournament diagram from tutorial" O.<> O.footer "by MM"

parseNoDiagram :: O.ParserInfo Commands
parseNoDiagram = flip O.info mod . (O.helper O.<*>) $ pure NoDiagram
  where
    mod = O.fullDesc O.<> O.header "Don't draw anything" O.<> O.footer "by MM"

