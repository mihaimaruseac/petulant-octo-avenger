module Tag (tagAndStore) where

import Data.Conduit

import Types

--tagAndStore :: Monad m => Conduit ChanneledHeaderRequest m ChanneledHeaderRequest
tagAndStore :: Conduit ChanneledHeaderRequest IO ChanneledHeaderRequest
tagAndStore = do
  (Just i) <- await
  yield i
