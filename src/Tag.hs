{-# LANGUAGE OverloadedStrings #-}

module Tag (tagAndStore) where

import qualified Data.ByteString as B

import Types

data DBCommand
  = Pass -- no actual data present
  | Game Int
  deriving Show

data TaggedInfo
  = Fail ChanneledHeaderRequest
  | OK DBCommand

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr
  show (OK Pass) = ""
  show (OK dbc) = show dbc

tagAndStore :: ChanneledHeaderRequest -> TaggedInfo
tagAndStore e@(rt, uri, rqhs, rqp, rphs, rpp)
  | uri == "game.php" = OK Pass
  | otherwise = Fail e
