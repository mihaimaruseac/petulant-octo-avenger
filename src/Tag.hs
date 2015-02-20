{-# LANGUAGE OverloadedStrings #-}

module Tag {-(tagAndStore)-} where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Control.Monad.State
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
parseOverviewStats = do
  cCmd <- parseRank "Competency:" C.readInt Competency
  fCmd <- parseRank "Progress:" readRank Faction
  apCmd <- parseLabelInfo "APs played:" readLongNumber AP
  credCmd <- parseLabelInfo "Credits:" readLongNumber Credits
  turnoverCmd <- parseLabelInfo "Turnover:" readLongNumber Turnover
  xpCmd <- parseLabelInfo "Experience:" readLongNumber XP
  aspCmd <- parseLabelInfo "ASPs:" C.readInt ASP
  atpCmd <- parseLabelInfo "ATPs:" readPardusDouble ATP
  return [cCmd, fCmd, apCmd, credCmd, turnoverCmd, xpCmd, aspCmd, atpCmd]

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
  t <- obtainFieldInfo [TagText title, TagText ""] $ build readFun
  return $ buildFun t
  where
    build rf t = extractTagText t >>= readAtStartIgnore rf

debug :: (Monad m, Show a) => a -> m DBCommand
debug = return . Debug . C.pack . show

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
  return (fromInteger i + (fromInteger f) / 100, r)

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

findTag :: Tag Payload -> StatsPSM (Tag Payload)
findTag = findTagN 1

findTagN :: Int -> Tag Payload -> StatsPSM (Tag Payload)
findTagN n t
  | n <= 0 = throwError $ CodingError "Should never require non-positive tags"
  | otherwise = do
    tags <- get
    case drop (n - 1) . sections (~== t) $ tags of
      (x:_) -> put (tail x) >> return (head x)
      _ -> throwError $ NoSuchTag n t
