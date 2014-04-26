{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Bits
import Data.Serialize
import Data.Word
import Network.Pcap
import System.Environment

import qualified Data.ByteString as B

import Debug.Trace

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
linkHdrLen DLT_LINUX_SLL = 16
linkHdrLen l = error $ concat ["Unknown link header ", show l]

mainCallback :: LinkLength -> CallbackBS
mainCallback hdrLen h@PktHdr{..} payload
  | hdrWireLength > hdrCaptureLength = incomplete h payload
  | otherwise = process $ B.drop hdrLen payload

incomplete :: CallbackBS
incomplete header _ = putStrLn $ "Incomplete packet captured" ++ show header

process :: ProcessPacket
process payload = print $ runGetPartial parseIP payload

-- http://tools.ietf.org/html/rfc791
data IP = IP
  { version :: Version
  , hlen :: Int
  } deriving Show

data Version = IPv4 | IPv6 deriving Show

mkVersion :: Word8 -> Version
mkVersion 4 = IPv4
mkVersion 6 = IPv6
mkVersion x = error $ concat ["Unknown IP version ", show x]

parseIP :: Get IP
parseIP = do
  vhl <- getWord8
  let v = mkVersion $ (vhl .&. 0xf0) `shiftR` 4
  let l = fromInteger . (4 *) . toInteger $ vhl .&. 0x0f
  return $ IP v l
