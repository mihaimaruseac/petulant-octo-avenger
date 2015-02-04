{-# LANGUAGE OverloadedStrings #-}

module Tag where --(tagAndStore) where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Maybe
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as C

import Errors
import Types

tagAndStore = undefined

{-
tagAndStore :: TaggedHeaderRequest -> StatsM [DBCommand]
tagAndStore thr@(_, uri, _, _, _, rpp)
  | uri == "msgframe.php" = return $ parseMsgFrame $ rpp
  | uri == "overview_stats.php" = return $ parseOverviewStats $ rpp
  | uri `elem` ["game.php", "menu.php"] = return []
  | otherwise = throwError $ UnhandledHTMLRequest thr

parseMsgFrame :: [Tag Payload] -> [DBCommand]
parseMsgFrame tags = case extract tags of
  Just (x, _) -> [POnline x]
  _ -> []
  where
    extract = \t -> searchByTags [imgTag, aTag, textTag] t >>= (extractPO . head)
    imgTag = TagOpen "img" [("id", "universe")]
    aTag = TagOpen "a" []
    textTag = TagText ""
    extractPO = C.readInt . last . C.words . fromTagText

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

{-
searchByTags :: [Tag Payload] -> [Tag Payload] -> Maybe [Tag Payload]
searchByTags [] = Just
searchByTags (t:ts) = \tags -> do
  tags' <- searchByTag t tags
  searchByTags ts tags'
  -}

findNthTag :: Int -> Tag Payload -> [Tag Payload] -> StatsM [Tag Payload]
findNthTag n t tags
  | n <= 0 = throwError $ OtherError "# Coding error! Should never require non-positive tags!"
  | otherwise = case drop (n - 1) . sections (~== t) $ tags of
    (x:xs) -> return x
    _ -> throwError $ NoSuchTag n t
