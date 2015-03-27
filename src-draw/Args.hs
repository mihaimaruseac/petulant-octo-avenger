module Args (parseArgs, Commands(..)) where

import Diagrams.Backend.CmdLine
import Options.Applicative

import Data.Monoid (mconcat)

type DO = (DiagramOpts, DiagramLoopOpts)

data Commands
  = Demo Int DO
  | VDemo Int DO
  | TDemo Int DO
  | Tournament DO
  | NoDiagram

instance Show Commands where
  show (Demo n (d, _)) = concat ["Demo ", show n, " ", show d]
  show (VDemo n (d, _)) = concat ["Vector ", show n, " ", show d]
  show (TDemo n (d, _)) = concat ["Trail ", show n, " ", show d]
  show (Tournament (d, _)) = concat ["Tournament ", show d]
  show NoDiagram = show "NoDiagram"

parseArgs :: IO Commands
parseArgs = execParser $ info (helper <*> parseModes) $
  buildMod "Draw diagrams"

parseModes :: Parser Commands
parseModes = build (parseDemo Demo) "Draw tutorial demo diagram" "demo"
         <|> build (parseDemo VDemo) "Draw vector demo diagram" "vector"
         <|> build (parseDemo TDemo) "Draw trails demo diagram" "trails"
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

parseDemo :: (Int -> DO -> Commands) -> String -> ParserInfo Commands
parseDemo ct d = flip info (buildMod d) . (helper <*>) $ ct
  <$> option auto
    ( short 'n'
   <> long "number"
   <> help "Demo number"
   <> metavar "INT"
   <> value 1
   <> showDefault
    )
  <*> parser

parseTournament :: String -> ParserInfo Commands
parseTournament d = flip info (buildMod d) . (helper <*>) $ Tournament <$> parser

parseNoDiagram :: String -> ParserInfo Commands
parseNoDiagram d = flip info (buildMod d) . (helper <*>) $ pure NoDiagram
