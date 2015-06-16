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
  | TSSMess TSSMessCommands DO
  -- Wars
  | Wars DO

data TSSMessCommands
  = GameInfo
  | RoleInfo
  | PlayerInfo
  | MechInfo
  deriving Show

instance Show Commands where
  show (Demo n (d, _)) = mconcat ["Demo ", show n, " ", show d]
  show (ADemo n (d, _)) = mconcat ["Arrows ", show n, " ", show d]
  show (TDemo n (d, _)) = mconcat ["Trail ", show n, " ", show d]
  show (VDemo n (d, _)) = mconcat ["Vector ", show n, " ", show d]
  show (Arrow (d, _)) = "Arrow " ++ show d
  show (Tournament (d, _)) = "Tournament " ++ show d
  show (Wars (d, _)) = "Wars " ++ show d
  show (TSSMess c (d, _)) = mconcat ["TSSMess", show c, " ", show d]
  show NoDiagram = "NoDiagram"

parseArgs :: IO Commands
parseArgs = execParser $ info (helper <*> parseModes) $
  buildMod "Draw diagrams"

parseModes :: Parser Commands
parseModes = build (parseDemo Demo) "Draw tutorial demo diagram" "demo"
         <|> build (parseDemo ADemo) "Draw arrows demo diagram" "arrows"
         <|> build (parseDemo TDemo) "Draw trails demo diagram" "trails"
         <|> build (parseDemo VDemo) "Draw vector demo diagram" "vector"
         <|> build (parseSingle Arrow) "Draw demo arrow diagram" "arrow"
         <|> build (parseSingle Tournament) "Draw demo tournament diagram" "tournament"
         <|> build (parseSingle Wars) "Pardus Wars diagrams" "wars"
         <|> build parseTSSMess "Pardus TSSMess diagrams" "tssmess"
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

parseSingle :: (DO -> a) -> String -> ParserInfo a
parseSingle f d = flip info (buildMod d) . (helper <*>) $ f <$> parser

parseNoDiagram :: String -> ParserInfo Commands
parseNoDiagram d = flip info (buildMod d) . (helper <*>) $ pure NoDiagram

parseTSSMess :: String -> ParserInfo Commands
parseTSSMess d = flip info (buildMod d) . (helper <*>) $ TSSMess
  <$> argument (error "1")
    ( help "Type of diagram: GameInfo | RoleInfo | PlayerInfo | MechInfo"
   <> metavar "TYPE"
    )
  <*> parser --undefined --flip info (buildMod d) . (helper <*>) $ pure TSSMess
