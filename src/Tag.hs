{-# LANGUAGE OverloadedStrings #-}

module Tag (tagAndStore) where

import Data.Maybe
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as C

import Types

import Debug.Trace

data DBCommand
  = POnline Int
  | MM [Tag Payload]
  deriving Show

data TaggedInfo
  = Fail ChanneledHeaderRequest
  | OK DBCommand

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr
  show (OK dbc) = show dbc

tagAndStore :: TaggedHeaderRequest -> [TaggedInfo]
tagAndStore (rt, uri, rqhs, rqp, rphs, rpp)
  | uri == "game.php" = []
  | uri == "menu.php" = []
  | uri == "msgframe.php" = map OK . parseMsgFrame $ resTags
  | otherwise = [Fail (rt, uri, rqhs, rqp, rphs, renderTags resTags)]
  where
    resTags = sanitize rpp

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

searchByTags :: [Tag Payload] -> [Tag Payload] -> Maybe [Tag Payload]
searchByTags [] = Just
searchByTags (t:ts) = \tags -> do
  tags' <- searchByTag t tags
  searchByTags ts tags'

searchByTag :: Tag Payload -> [Tag Payload] -> Maybe [Tag Payload]
searchByTag t = listToMaybe . sections (~== t)
