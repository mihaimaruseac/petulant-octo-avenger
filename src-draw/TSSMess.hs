module TSSMess (doTssMess) where

import Control.Lens hiding ((#))
--import Data.Text
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (view)

import Args
import Utils

import TSSMess.Data
import TSSMess.Types

doTssMess :: TSSMessCommands -> (Diagram B -> IO ()) -> IO ()
doTssMess GameInfo rf = doTMGI rf
doTssMess m rf = do
  fImgs <- mapM loadImage
    [ "src-draw/res/wars/sign_fed_64x64.png"
    , "src-draw/res/wars/sign_emp_64x64.png"
    , "src-draw/res/wars/sign_uni_64x64.png"
    ]
  rf undefined

doTMGI :: (Diagram B -> IO ()) -> IO ()
doTMGI rf = do
  rf $ gameInfos

gameInfos :: Diagram B
gameInfos = gameInfo $ head games

gameInfo :: Game -> Diagram B
gameInfo g = textBox 900 50 green (g ^. dm) --undefined

textBox :: Double -> Double -> Colour Double -> String -> Diagram B
textBox w h c t = text t # fc black <> frameBox
  where
    frameBox = rect w h # bg c # lc c -- # alignY (-0.6)

