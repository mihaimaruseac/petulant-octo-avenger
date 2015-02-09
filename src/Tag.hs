{-# LANGUAGE OverloadedStrings #-}

module Tag {-(tagAndStore)-} where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as C

import Errors
import Types

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
  | uri `elem` ["game.php", "menu.php"] = return []
  | otherwise = throwError $ UnhandledHTMLRequest thr

parseMsgFrame :: StatsPSM [DBCommand]
parseMsgFrame = obtainFieldInfo tags build
  where
    tags = [TagOpen "img" [("id", "universe")], TagOpen "a" [], TagText ""]
    build t = extractTagText t >>= readAtEnd C.readInt >>= return . return . POnline

parseOverviewStats :: StatsPSM [DBCommand]
parseOverviewStats = undefined

{-
parseOverviewStats :: [Tag Payload] -> [DBCommand]
parseOverviewStats t = (flip evalState) t $ do
  parseFactionLevels kTags build
  where
    kTags = [TagText "Competency:", TagOpen "td" [], TagOpen "img" []]
    build tg = [Competency . fst . fromJust . C.readInt . fromAttrib "title" $ tg]

parseFactionLevels :: [Tag Payload] -> (Tag Payload -> a) -> State [Tag Payload] a
parseFactionLevels kTags build = do
  mtags <- fmap (searchByTags kTags) get
  case mtags of
    Just (t:tags) -> do
      put tags
      return $ build t
    _ -> fail ""
    -}

debug :: (Show a) => a -> StatsM [DBCommand]
debug =  return . return . Debug . C.pack . show

readAtStartM :: (Payload -> Maybe (a, Payload)) -> StatsSM Payload a
readAtStartM f = do
  p <- get
  case f p of
    Just (i, p') -> put p' >> return i
    Nothing -> throwError $ CannotParseTagContent p

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
