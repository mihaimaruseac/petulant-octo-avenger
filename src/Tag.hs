{-# LANGUAGE OverloadedStrings #-}

module Tag (tagAndStore) where

import qualified Data.ByteString as B

import Types

data DBCommand
  = Game Int
  deriving Show

data TaggedInfo
  = Fail ChanneledHeaderRequest
  | OK DBCommand

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr
  show (OK dbc) = show dbc

tagAndStore :: ChanneledHeaderRequest -> [TaggedInfo]
tagAndStore e@(rt, uri, rqhs, rqp, rphs, rpp)
  | uri == "game.php" = map (OK . Game) [1, 2, 42]
  | uri == "menu.php" = []
  | uri == "msgframe.php" = [] -- TODO: parse "Players online: <int>" field
  | otherwise = [Fail e]
