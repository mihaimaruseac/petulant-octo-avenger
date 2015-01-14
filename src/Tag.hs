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

tagAndStore :: ChanneledHeaderRequest -> TaggedInfo
tagAndStore e@(rt, uri, rqhs, rqp, rphs, rpp)
  | otherwise = Fail e
