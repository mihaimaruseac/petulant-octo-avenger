module Args (parseArgs, DO, Commands(..), TSSMessCommands(..)) where

import Diagrams.Backend.CmdLine
import Options.Applicative
import Text.Printf

import Data.Monoid (mconcat, (<>))

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
  -- Activity
  | Activity FilePath Int DO

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
  show (TSSMess c (d, _)) = mconcat ["TSSMess ", show c, " ", show d]
  show (Activity dir off (d, _)) = printf "Activity %s %d %s" dir off $ show d
  show NoDiagram = "NoDiagram"

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
         <|> buildSP (parseActivity Activity) "Pardus war activity diagrams" "activity"
         <|> buildSP (parsePure NoDiagram) "Don't draw anything" "nodia"

buildMod :: String -> InfoMod a
buildMod d = mconcat
  [ fullDesc
  , header "Generic diagram drawer"
  , footer "© 2015 Mihai Maruseac"
  , progDesc d
  ]

buildParserHelper :: String -> Parser a -> ParserInfo a
buildParserHelper d = flip info (buildMod d) . (helper <*>)

buildSP :: (p -> ParserInfo a) -> p -> String -> Parser a
buildSP parseFun descr c = subparser (command c (parseFun descr) <> metavar c)

parseSingle :: (Parseable b) => (b -> a) -> String -> ParserInfo a
parseSingle f d = buildParserHelper d $ f <$> parser

parsePure :: a -> String -> ParserInfo a
parsePure f d = buildParserHelper d $ pure f

parseDemo :: (Int -> DO -> Commands) -> String -> ParserInfo Commands
parseDemo ct d = buildParserHelper d $ ct
  <$> option auto
    ( short 'n'
   <> long "number"
   <> help "Demo number"
   <> metavar "INT"
   <> value 1
   <> showDefault
    )
  <*> parser

parseActivity :: (FilePath -> Int -> DO -> Commands) -> String -> ParserInfo Commands
parseActivity ct d = buildParserHelper d $ ct
  <$> strOption
    ( short 'd'
   <> long "datadir"
   <> help "Directory of data files (data,sides,predictions)"
   <> metavar "DIRECTORY"
   <> value "./src-draw/WarActivity/war2017"
   <> showDefault
    )
  <*> option auto
    ( short 'j'
   <> long "offset"
   <> help "Offset"
   <> metavar "OFFSET"
   <> value 0
   <> showDefault
    )
  <*> parser

parseTSSMess :: String -> ParserInfo Commands
parseTSSMess d = buildParserHelper d $ TSSMess <$> subparse <*> parser
  where
    subparse = buildSP (parsePure GameInfo) "Game info" "g"
           <|> buildSP (parsePure RoleInfo) "Role info" "r"
           <|> buildSP (parsePure PlayerInfo) "Player info" "p"
           <|> buildSP (parsePure MechInfo) "Special mechanics" "m"
