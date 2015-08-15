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
gameInfos = vcat . map gameInfo $ games

gameInfo :: Game -> Diagram B
gameInfo g = vcat
  [ mkTxt bold   $ concat [g ^. title, " (", g ^. dm, ")"]
  , mkTxt italic $ concat ["\"", g ^. quote, "\""]
  , mkTxt id     $ concat [show $ g ^. announcementDate, " ", show $ g ^. startDate, " ", show $ g ^. endDate]
  ]
  where
    w = 900
    mkTxt style s = textBox 14 s style w 30 black green

textBox :: Double -- font size
  -> String -- text
  -> (Diagram B -> Diagram B)
  -> Double -- width of box
  -> Double -- height of box
  -> Colour Double -- foreground
  -> Colour Double -- background
  -> Diagram B
textBox s t td w h c bc = text t # fc c # fontSizeG s # td <> frameBox
  where
    frameBox = rect w h # bg bc # lc bc
