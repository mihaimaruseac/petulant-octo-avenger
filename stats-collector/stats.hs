{-
 - Extract statistics about the Artemis universe of Pardus.
 -}
import Network.Pcap

main = do
  putStrLn "Opening capture device"
  h <- openLive "any" 0 False 0
  pRead <- loopBS h (- 1) cb
  putStrLn $ "Read " ++ show pRead ++ " packets"

cb :: CallbackBS
cb h bs = do
  print h
  print bs
  return ()
