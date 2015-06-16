module Args (parseArgs, DO, Commands(..)) where

import Diagrams.Backend.CmdLine
import Options.Applicative

import Data.Monoid (mconcat)

type DO = (DiagramOpts, DiagramLoopOpts)

data Commands
  -- demos first
  = Demo Int DO
  | ADemo Int DO
  | TDemo Int DO
  | VDemo Int DO
  -- special demos
  | Arrow DO
  | Tournament DO
  -- demo for no diagram
  | NoDiagram
  -- TSS mess
  | TSSMess TSSMessCommands
  -- Wars
  | Wars DO

data TSSMessCommands
  = GameInfo DO
  | RoleInfo DO
  | PlayerInfo DO
  | MechInfo DO

instance Show Commands where
  show (Demo n (d, _)) = mconcat ["Demo ", show n, " ", show d]
  show (ADemo n (d, _)) = mconcat ["Arrows ", show n, " ", show d]
  show (TDemo n (d, _)) = mconcat ["Trail ", show n, " ", show d]
  show (VDemo n (d, _)) = mconcat ["Vector ", show n, " ", show d]
  show (Arrow (d, _)) = "Arrow " ++ show d
  show (Tournament (d, _)) = "Tournament " ++ show d
  show (Wars (d, _)) = "Wars " ++ show d
  show (TSSMess c) = "TSSMess " ++ show c
  show NoDiagram = "NoDiagram"

instance Show TSSMessCommands where
  show (GameInfo (d, _)) = "GameInfo " ++ show d
  show (RoleInfo (d, _)) = "RoleInfo " ++ show d
  show (PlayerInfo (d, _)) = "PlayerInfo " ++ show d
  show (MechInfo (d, _)) = "MechInfo " ++ show d

parseArgs :: IO Commands
parseArgs = execParser $ info (helper <*> parseModes) $
  buildMod "Draw diagrams"

parseModes :: Parser Commands
parseModes = buildSP (parseDemo Demo) "Draw tutorial demo diagram" "demo"
         <|> buildSP (parseDemo ADemo) "Draw arrows demo diagram" "arrows"
         <|> buildSP (parseDemo TDemo) "Draw trails demo diagram" "trails"
         <|> buildSP (parseDemo VDemo) "Draw vector demo diagram" "vector"
         <|> buildSP (parseSingle Arrow) "Draw demo arrow diagram" "arrow"
         <|> buildSP (parseSingle Tournament) "Draw demo tournament diagram" "tournament"
         <|> buildSP (parseSingle Wars) "Pardus Wars diagrams" "wars"
         <|> buildSP parseTSSMess "Pardus TSSMess diagrams" "tssmess"
         <|> buildSP parseNoDiagram "Don't draw anything" "nodia"

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

parseSingle :: (Parseable b) => (b -> a) -> String -> ParserInfo a
parseSingle f d = flip info (buildMod d) . (helper <*>) $ f <$> parser

buildSP :: (p -> ParserInfo a) -> p -> String -> Parser a
buildSP parseFun descr c = subparser (command c (parseFun descr) <> metavar c)

parseNoDiagram :: String -> ParserInfo Commands
parseNoDiagram d = flip info (buildMod d) . (helper <*>) $ pure NoDiagram

parseTSSMess :: String -> ParserInfo Commands
parseTSSMess d = flip info (buildMod d) . (helper <*>) $ TSSMess <$> subparse
  where
    subparse = buildSP (parseSingle GameInfo) "Game info" "g"
           <|> buildSP (parseSingle RoleInfo) "Role info" "r"
           <|> buildSP (parseSingle PlayerInfo) "Player info" "p"
           <|> buildSP (parseSingle MechInfo) "Special mechanics" "m"
