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
  buildMod "Draw diagrams"

parseModes :: Parser Commands
parseModes = build parseDemo "Draw demo diagram from tutorial" "demo"
         <|> build parseTournament "Draw demo tournament diagram" "tournament"
         <|> build parseNoDiagram "Don't draw anything" "nodia"
  where
    build p d c = subparser (command c (p d) <> metavar c)

buildMod :: String -> InfoMod a
buildMod d = mconcat
  [ fullDesc
  , header "Generic diagram drawer"
  , footer "© 2015 Mihai Maruseac"
  , progDesc d
  ]

parseDemo :: String -> ParserInfo Commands
parseDemo d = flip info (buildMod d) . (helper <*>) $ Demo
  <$> option auto
    ( short 'n'
   <> long "number"
   <> help "Demo number"
   <> metavar "INT"
   <> value 0
   <> showDefault
    )
  <*> parser

parseTournament :: String -> ParserInfo Commands
parseTournament d = flip info (buildMod d) . (helper <*>) $ Tournament <$> parser

parseNoDiagram :: String -> ParserInfo Commands
parseNoDiagram d = flip info (buildMod d) . (helper <*>) $ pure NoDiagram
