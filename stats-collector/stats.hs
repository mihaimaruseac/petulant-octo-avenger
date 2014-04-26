{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Pcap
import Numeric
import System.Environment

import qualified Data.ByteString as B

gSnapshotSize :: Int
gSnapshotSize = 1000000000

gDefaultUniverse :: String
gDefaultUniverse = "artemis"

type LinkLength = Int
type Payload = B.ByteString
type ProcessPacket = Payload -> IO ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> statsOn gDefaultUniverse
    u:_ -> statsOn u

statsOn :: String -> IO ()
statsOn universe = do
  handle <- openLive "any" gSnapshotSize False 0
  setFilter handle (buildFilter universe) True 0
  link <- datalink handle
  let hdrLen = linkHdrLen link
  putStrLn $ concat ["Capturing on ", universe]
  putStrLn "Press ^C twice to end"
  pRead <- loopBS handle (- 1) $ mainCallback hdrLen
  putStrLn $ "Read " ++ show pRead ++ " packets"

buildFilter :: String -> String
buildFilter universe = concat ["host ", universe, ".pardus.at"]

linkHdrLen :: Link -> LinkLength
linkHdrLen DLT_LINUX_SLL = 12
linkHdrLen l = error $ concat ["Unknown link header ", show l]

mainCallback :: LinkLength -> CallbackBS
mainCallback hdrLen h@PktHdr{..} payload
  | hdrWireLength > hdrCaptureLength = incomplete h payload
  | otherwise = process $ B.drop hdrLen payload

incomplete :: CallbackBS
incomplete header _ = putStrLn $ "Incomplete packet captured" ++ show header

process :: ProcessPacket
process payload
  | B.pack [0, 0, 8, 0] == B.take 4 payload = processIP $ B.drop 4 payload
  | otherwise = error $ concat ["Unknown second layer protocol ", show . B.unpack . B.take 4 $ payload]

processIP :: ProcessPacket
processIP payload
  | False = undefined
  | otherwise = putStrLn . concatMap (flip showHex "") . B.unpack $ payload
