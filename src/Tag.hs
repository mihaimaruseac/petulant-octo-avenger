module Tag (tagAndStore) where

import Types

data TaggedInfo
  = Fail ChanneledHeaderRequest

instance Show TaggedInfo where
  show (Fail chr) = '#' : ' ' : show chr

tagAndStore :: ChanneledHeaderRequest -> IO TaggedInfo
tagAndStore e = do
  print $ Fail e
  return $ Fail e
