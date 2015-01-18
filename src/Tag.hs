{-# LANGUAGE OverloadedStrings #-}

module Tag (tagAndStore) where

import Text.HTML.TagSoup

import qualified Data.ByteString as B

import Types

data DBCommand
  = Game Int
  | MM [Tag Payload]
  deriving Show

data TaggedInfo
  = Fail ChanneledHeaderRequest
  | OK DBCommand

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr
  show (OK dbc) = show dbc

tagAndStore :: TaggedHeaderRequest -> [TaggedInfo]
tagAndStore e@(rt, uri, rqhs, rqp, rphs, rpp)
  | uri == "game.php" = []
  | uri == "menu.php" = []
  | uri == "msgframe.php" = map OK $ parseMsgFrame rpp
  | otherwise = [Fail (rt, uri, rqhs, rqp, rphs, renderTags rpp)]

parseMsgFrame :: [Tag Payload] -> [DBCommand]
parseMsgFrame rpp = [MM rpp] -- TODO: parse "Players online: <int>" field
