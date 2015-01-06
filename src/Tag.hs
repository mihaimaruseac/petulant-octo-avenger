module Tag (tagAndStore) where

import Types

data TaggedInfo
  = Fail ChanneledHeaderRequest

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr

tagAndStore :: ChanneledHeaderRequest -> TaggedInfo
tagAndStore = Fail
