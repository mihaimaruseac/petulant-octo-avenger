{-# LANGUAGE LambdaCase #-}

module WarActivity (doWA) where

import Data.Colour.Names
import Data.List.Split
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (view)
import System.FilePath
import Text.Printf

import qualified Data.Map.Strict as M

doWA :: FilePath -> Int -> (Diagram B -> IO ()) -> IO ()
doWA dir off rf = do
  preds <- readPredictions $ dir </> "predictions"
  datas <- readMarkers     $ dir </> "data"
  sides <- readSides       $ dir </> "sides"
  rf $ draw preds datas sides off

readPredictions :: FilePath -> IO (M.Map String (M.Map Int Double))
readPredictions f =
  M.fromListWith h . map (g . splitOn ";") . lines <$> readFile f
  where
    g [a,b,c] = (a, (M.singleton (read b) $ read c))
    g x = error $ printf "Received unexpected %s" $ show x
    h m1 m2 = m1 `M.union` m2

readMarkers :: FilePath -> IO (M.Map String [Int])
readMarkers f =
  M.fromListWith (++) . map (g . splitOn ";") . lines <$> readFile f
  where
    g [a,b] = (a, [read b])
    g x = error $ printf "Received unexpected %s" $ show x

readSides :: FilePath -> IO (M.Map String (Colour Double))
readSides f = M.fromList . map (g . splitOn ";") . lines <$> readFile f
  where
    g [a,b] = (a, h b)
    g x = error $ printf "Received unexpected %s" $ show x
    h = \case
      "red"  -> darkred
      "blue" -> blue
      "gray" -> thistle
      s      -> error $ printf "Unhandled side %s" s

draw
  :: M.Map String (M.Map Int Double) -- ^ predictions
  -> M.Map String [Int]              -- ^ markers
  -> M.Map String (Colour Double)    -- ^ colors
  -> Int                             -- ^ offset
  -> Diagram B                       -- ^ resulting diagram
draw p m c o = mconcat
  [ rect (30 * w) (100 * h) # lw 1 # lc black # translate (r2 ( 14 * w + 320, dy))
  , rect (31 * w) (100 * h) # lw 1 # lc black # translate (r2 ( 75 * w + 328, dy))
  , rect (31 * w) (100 * h) # lw 1 # lc black # translate (r2 (136 * w + 328, dy))
  , rect (31 * w) (100 * h) # lw 1 # lc black # translate (r2 (197 * w + 328, dy))
  , rect (28 * w) (100 * h) # lw 1 # lc black # translate (r2 (259 * w + 304, dy))
  , rect (30 * w) (100 * h) # lw 1 # lc black # translate (r2 (318 * w + 320, dy))
    -- war duration
  , rect (71 * w) (100 * h) # lw 1 # lc coral # translate (r2 (254 * w + 648, dy))
    -- activity
  , vcat $ map f $ take 100 $ drop o $ M.keys p
  ]
  where
    f n = drawLine (p M.! n) (m M.! n) (c M.! n) n
    w = 16
    h = 32
    dy = -50 * h + 16

drawLine
  :: M.Map Int Double -- ^ prediction
  -> [Int]            -- ^ marker
  -> Colour Double    -- ^ color
  -> String           -- ^ name
  -> Diagram B        -- ^ resulting diagram
drawLine ps ms c n = mconcat
  [ textName ||| hcat [ bb d | d <- ds ]
  , rect ww h # fc white # lw 1 # translate (r2 ((ww + wt)/2, 0))
  ]
  where
    bb d
      | d `elem` ms = text "#" # fontSizeL tf # fc black <> aBox (ps M.! d)
      | otherwise = aBox (ps M.! d)
    textName = text n # fontSizeL (tf-2) # fc black <> rect wt h # bg c # lw none
    ds = M.keys ps
    maxd = fromIntegral $ maximum ds
    ww = (1 + maxd) * w
    w = 16
    h = 32
    wt = 160
    tf = 12
    aBox p = rect w h # fcA c' # lw none
      where c' = c `withOpacity` getOpcty p

getOpcty :: Double -> Double
getOpcty p
  | p < 0.2 = 0
  | p < 0.4 = 0.5
  | p < 0.6 = 0.7
  | p < 0.8 = 0.8
  | p < 0.9 = 0.9
  | otherwise = 1
