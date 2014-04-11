{-
 - Extract statistics about the Artemis universe of Pardus.
 -}
import Network.Pcap

gSnapshotSize = 1000000000

main = do
  devs <- findAllDevs
  print devs
  putStrLn "Opening capture device"
  h <- openLive "any" gSnapshotSize False 0
  pRead <- loopBS h (- 1) cb
  putStrLn $ "Read " ++ show pRead ++ " packets"

cb :: CallbackBS
cb h bs = do
  print h
  print bs
  return ()
