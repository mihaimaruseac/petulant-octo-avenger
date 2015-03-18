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
parseArgs = execParser $ info (helper <*> parseModes) $
  buildMod "Generic diagram drawer" "Draw diagrams"

buildMod :: String -> String -> InfoMod a
buildMod h d = mconcat
  [ fullDesc
  , header h
  , footer "© 2015 Mihai Maruseac"
  , progDesc d
  ]

-- parser for all modes
parseModes :: Parser Commands
parseModes = build "demo" parseDemo
         <|> build "tournament" parseTournament
         <|> build "nodia" parseNoDiagram
  where
    build c f = subparser (command c f <> metavar c)

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

