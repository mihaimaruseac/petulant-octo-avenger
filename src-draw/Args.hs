module Args (parseArgs, Commands(..)) where

import Diagrams.Backend.CmdLine
import Options.Applicative

import Data.Monoid (mconcat)

type DO = (DiagramOpts, DiagramLoopOpts)

data Commands
  = Demo Int DO
  | Tournament DO
  | NoDiagram

instance Show Commands where
  show (Demo n (d, _)) = concat ["Demo ", show n, " ", show d]
  show (Tournament (d, _)) = concat ["Tournament ", show d]
  show NoDiagram = show "NoDiagram"

parseArgs :: IO Commands
parseArgs = execParser $ info (helper <*> parseModes) $ mconcat
    [ fullDesc
    , header "Generic diagram drawer"
    , footer "by MM"
    , progDesc "Draw diagrams"
    ]

-- parser for all modes
parseModes :: Parser Commands
parseModes = subparser (command "demo" parseDemo <> metavar "demo")
       <|> subparser (command "tournament" parseTournament <> metavar "tournament")
       <|> subparser (command "nodia" parseNoDiagram <> metavar "nodia")

-- individual parsers
parseDemo :: ParserInfo Commands
parseDemo = flip info mdf . (helper <*>) $ Demo
  <$> option auto
      (    short 'n'
      <> long "number"
      <> help "Demo number"
      <> metavar "INT"
      <> value 14
      <> showDefault
      -- <> completer (bashCompleter "smth") -- disabled because of not being implemented
      )
  <*> parser
  where
    mdf = fullDesc <> header "Draw demo diagram from tutorial" <> footer "by MM"

parseTournament :: ParserInfo Commands
parseTournament = flip info mdf . (helper <*>) $ Tournament <$> parser
  where
    mdf = fullDesc <> header "Draw demo tournament diagram from tutorial" <> footer "by MM"

parseNoDiagram :: ParserInfo Commands
parseNoDiagram = flip info mdf . (helper <*>) $ pure NoDiagram
  where
    mdf = fullDesc <> header "Don't draw anything" <> footer "by MM"

