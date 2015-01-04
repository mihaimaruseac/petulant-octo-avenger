module Tag (tagAndStore) where

import Types

tagAndStore :: ChanneledHeaderRequest -> IO ChanneledHeaderRequest
tagAndStore e = do
  print e
  return e
