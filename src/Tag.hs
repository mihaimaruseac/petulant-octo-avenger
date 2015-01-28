{-# LANGUAGE OverloadedStrings #-}

module Tag (tagAndStore) where

import Control.Monad.State
import Data.Maybe
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as C

import Types

import Debug.Trace

tagAndStore :: TaggedHeaderRequest -> [TaggedInfo]
tagAndStore (rt, uri, rqhs, rqp, rphs, rpp)
  | uri == "game.php" = []
  | uri == "menu.php" = []
  | uri == "msgframe.php" = map OK . parseMsgFrame $ resTags
  | uri == "overview_stats.php" = map OK . parseOverviewStats $ resTags
  | otherwise = [Fail (rt, uri, rqhs, rqp, rphs, render resTags)]
  where
    resTags = sanitize rpp

render :: [Tag Payload] -> Payload
render tags = renderTagsOptions options tags
  where
    options = RenderOptions
      { optEscape = id
      , optMinimize = const False
      , optRawTag = const False
      }

sanitize :: [Tag Payload] -> [Tag Payload]
sanitize = filter (/= TagText "") . map sanitizeTag

sanitizeTag :: Tag Payload -> Tag Payload
sanitizeTag t
  | isTagText t = TagText . C.unwords . C.words . fromTagText $ t
  | otherwise = t

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
parseOverviewStats = evalState parseFactionLevels --undefined --concat . ([parseFactionLevels] <*>)

parseFactionLevels :: State [Tag Payload] [DBCommand]
parseFactionLevels = do
  let kTags = [TagText "Competency:", TagOpen "td" [], TagOpen "img" []]
  mtags <- fmap (searchByTags kTags) get
  case mtags of
    Just (t:tags) -> do
      put tags
      return [Competency . fst . fromJust . C.readInt . fromAttrib "title" $ t]
    _ -> return []

searchByTags :: [Tag Payload] -> [Tag Payload] -> Maybe [Tag Payload]
searchByTags [] = Just
searchByTags (t:ts) = \tags -> do
  tags' <- searchByTag t tags
  searchByTags ts tags'

searchByTag :: Tag Payload -> [Tag Payload] -> Maybe [Tag Payload]
searchByTag t = listToMaybe . sections (~== t)
