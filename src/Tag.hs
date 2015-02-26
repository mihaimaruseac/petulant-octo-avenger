{-# LANGUAGE OverloadedStrings #-}

module Tag {-(tagAndStore)-} where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.List
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as C

import Errors
import Types

import Debug.Trace

type StatsSM s a = StateT s StatsM a
type StatsPSM a = StatsSM [Tag Payload] a

runStatsSM :: s -> StatsSM s a -> StatsM (a, s)
runStatsSM s m = runStateT m s

evalStatsSM :: s -> StatsSM s a -> StatsM a
evalStatsSM s m = case runStatsSM s m of
  Right (a, _) -> return a
  Left e -> throwError e

tagAndStore :: TaggedHeaderRequest -> StatsM [DBCommand]
tagAndStore thr@(_, uri, _, _, _, rpp)
  | uri == "msgframe.php" = evalStatsSM rpp parseMsgFrame
  | uri == "overview_stats.php" = evalStatsSM rpp parseOverviewStats
  | uri `elem` ignorabimus = return []
  | otherwise = throwError $ UnhandledHTMLRequest thr
  where
    ignorabimus =
      [ "game.php"
      , "menu.php"
      , "overview_jobs.php"
      , "overview_payment_log.php"
      , "overview_buildings.php"
      , "overview_ship.php"
      ]


parseMsgFrame :: StatsPSM [DBCommand]
parseMsgFrame = obtainFieldInfo tags build
  where
    tags = [TagOpen "img" [("id", "universe")], TagOpen "a" [], TagText ""]
    build t = return . POnline <$> (extractTagText t >>= readAtEnd C.readInt)

parseOverviewStats :: StatsPSM [DBCommand]
parseOverviewStats = sequence
  [ parseRank "Competency:" C.readInt Competency
  , parseRank "Progress:" readRank Faction
  , parseLabelInfo "APs played:" readLongNumber AP
  , parseLabelInfo "Credits:" readLongNumber Credits
  , parseLabelInfo "Turnover:" readLongNumber Turnover
  , parseLabelInfo "Experience:" readLongNumber XP
  , parseLabelInfo "ASPs:" C.readInt ASP
  , parseLabelInfo "ATPs:" readPardusDouble ATP
  , parseReputation
  , parseLabelInfo "Other pilots killed:" C.readInt PilotKill
  , parseLabelInfo "War Medals earned:" C.readInt WarMedals
  , parseLabelInfo "Own ship destroyed:" C.readInt PilotDeath
  , parseLabelInfo "Bounties collected:" C.readInt Bounties
  , parseLabelInfo "'Kill' Bounties:" C.readInt KillBounties
  , parseLabelInfo "'Destroy' Bounties:" C.readInt DestroyBounties
  , parseLabelInfo "Total NPCs killed:" readLongNumber NPCKill
  , parseLabelInfo "Combat Ribbons earned:" C.readInt Ribbons
  , parseKills
  ]

parseRank :: Payload -> (Payload -> Maybe (a, Payload)) -> (a -> Int -> DBCommand) -> StatsPSM DBCommand
parseRank title readFun buildFun = do
  name <- obtainFieldInfo tags (build readFun)
  val <- obtainFieldInfoN [(2, TagOpen "td" [])] (build C.readInt)
  return $ buildFun name val
  where
    tags = [TagText title, TagOpen "td" [], TagOpen "img" []]
    build rf t = extractAttrib "title" t >>= readAtStartIgnore rf

parseLabelInfo :: Payload -> (Payload -> Maybe (a, Payload)) -> (a -> DBCommand) -> StatsPSM DBCommand
parseLabelInfo title readFun buildFun = do
  t <- readTagThenTextStart (TagText title) readFun
  return $ buildFun t

parseReputation :: StatsPSM DBCommand
parseReputation = do
  f:e:u:a:_ <- mapM rd ["fed", "emp", "uni", "avg"]
  return $ Rep f e u a
  where
    rd x = readTagThenTextStart (buildT x) C.readInt
    buildT s = TagOpen "span" [("id", C.concat ["rep", s, "_current"])]

parseKills :: StatsPSM DBCommand
parseKills = do
  s <- getUntilTag (TagOpen "td" [("valign", "top"), ("align", "center")])
  debug $ filter (~== (TagText "" :: Tag Payload)) s

debug :: (Monad m, Show a) => a -> m DBCommand
debug = return . Debug . C.pack . show

readTagThenTextStart :: Tag Payload -> (Payload -> Maybe (a, Payload)) -> StatsPSM a
readTagThenTextStart tag rf = obtainFieldInfo [tag, TagText ""] build
  where
    build t = extractTagText t >>= readAtStartIgnore rf

-- TODO: make it work by returning rank level instead of rank name
readRank :: Payload -> Maybe (Payload, Payload)
readRank s = Just (head . C.words $ s, "") -- ignore the remaining of the string

readLongNumber :: Payload -> Maybe (Int, Payload)
readLongNumber = C.readInt . C.concat . C.split ','

readPardusDouble :: Payload -> Maybe (Double, Payload)
readPardusDouble x = do
  (i, p) <- C.readInteger x
  guard $ C.head p == '.'
  (f, r) <- C.readInteger $ C.drop 1 p
  return (fromInteger i + fromInteger f / 100, r)

readAtStartIgnore :: (Payload -> Maybe (a, Payload)) -> Payload -> StatsM a
readAtStartIgnore f w = case f w of
  Just (i, _) -> return i
  Nothing -> throwError $ CannotParseTagContent w

readAtEnd :: (Payload -> Maybe (a, Payload)) -> Payload -> StatsM a
readAtEnd f w = case f w' of
  Just (i, _) -> return i
  Nothing -> throwError $ CannotParseTagContent w
  where
    w' = last . C.words $ w

extractTagText :: Tag Payload -> StatsM Payload
extractTagText tag
  | not (isTagText tag) = throwError $ CodingError "Should always get tag text from TagText tags"
  | otherwise = return $ fromTagText tag

extractAttrib :: Payload -> Tag Payload -> StatsM Payload
extractAttrib attrib tag
  | not (isTagOpen tag) = throwError $ CodingError "Should always get attribute from open tags"
  | t == "" = throwError $ NoAttribute tag attrib
  | otherwise = return t
  where
    t = fromAttrib attrib tag

obtainFieldInfo :: [Tag Payload] -> (Tag Payload -> StatsM a) -> StatsPSM a
obtainFieldInfo tgs f = searchByTags tgs >>= (lift . f)

obtainFieldInfoN :: [(Int, Tag Payload)] -> (Tag Payload -> StatsM a) -> StatsPSM a
obtainFieldInfoN tgs f = searchByTagsN tgs >>= (lift . f)

searchByTags :: [Tag Payload] -> StatsPSM (Tag Payload)
searchByTags [] = throwError $ CodingError "Should always have at least on tag to search for"
searchByTags tgs = foldM (flip $ const . findTag) undefined tgs

searchByTagsN :: [(Int, Tag Payload)] -> StatsPSM (Tag Payload)
searchByTagsN [] = throwError $ CodingError "Should always have at least on tag to search for"
searchByTagsN tgs = foldM (flip $ const . uncurry findTagN) undefined tgs

getUntilTag :: Tag Payload -> StatsPSM [Tag Payload]
getUntilTag = getUntilTagN 1

findTag :: Tag Payload -> StatsPSM (Tag Payload)
findTag = findTagN 1

getUntilTagN :: Int -> Tag Payload -> StatsPSM [Tag Payload]
getUntilTagN n t
  | n <= 0 = throwError $ CodingError "Should never require non-positive tags"
  | otherwise = do
    tags <- get
    case drop (n - 1). fst . partition ((~==t) . snd) . zip [0..] $ tags of
      ((x,_):_) -> put (drop x tags) >> return (take x tags)
      _ -> throwError $ NoSuchTag n t

findTagN :: Int -> Tag Payload -> StatsPSM (Tag Payload)
findTagN n t
  | n <= 0 = throwError $ CodingError "Should never require non-positive tags"
  | otherwise = do
    tags <- get
    case drop (n - 1) . sections (~== t) $ tags of
      (x:_) -> put (tail x) >> return (head x)
      _ -> throwError $ NoSuchTag n t
