{-# LANGUAGE OverloadedStrings #-}

{-
 - Extract statistics about the Artemis universe of Pardus.
 -}
import Network.Pcap
import System.Environment

gSnapshotSize :: Int
gSnapshotSize = 1000000000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> statsOn "artemis"
    u:_ -> statsOn u

statsOn :: String -> IO ()
statsOn universe = do
  handle <- openLive "any" gSnapshotSize False 0
  setFilter handle (buildFilter universe) True 0
  pRead <- loopBS handle (- 1) mainCallback
  putStrLn $ "Read " ++ show pRead ++ " packets"

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

mainCallback :: CallbackBS
mainCallback header payload
  | hdrWireLength header > hdrCaptureLength header = incomplete header
  | otherwise = process header payload

incomplete :: PktHdr -> IO ()
incomplete header = putStrLn $ "Incomplete packet captured" ++ show header

process :: CallbackBS
process header payload = do
    print header
    --print payload
