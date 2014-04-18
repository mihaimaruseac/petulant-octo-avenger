{-# LANGUAGE OverloadedStrings #-}

{-
 - Extract statistics about the Artemis universe of Pardus.
 -}
import Network.Pcap

gSnapshotSize = 1000000000

main = do
  putStrLn "Opening capture device"
  h <- openLive "any" gSnapshotSize False 0
  setFilter h "host artemis.pardus.at" True 0
  pRead <- loopBS h (- 1) cb
  putStrLn $ "Read " ++ show pRead ++ " packets"

cb :: CallbackBS
cb h bs
  | hdrWireLength h > hdrCaptureLength h = putStrLn $ "Incomplete packet captured" ++ show h
  | otherwise = do
    print h
    --print bs
    return ()
