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
doTssMess _ rf = do
  _fImgs <- mapM loadImage
    [ "src-draw/res/wars/sign_fed_64x64.png"
    , "src-draw/res/wars/sign_emp_64x64.png"
    , "src-draw/res/wars/sign_uni_64x64.png"
    ]
  rf undefined

doTMGI :: (Diagram B -> IO ()) -> IO ()
doTMGI rf = do
  rf $ gameInfos

gameInfos :: Diagram B
gameInfos = vcat $ spaceSection : map gameInfo games

gameInfo :: Game -> Diagram B
gameInfo g = vcat
  [ titleSection g
  , quoteSection g
  , datesSection g
  , playerTableSection g
  , spaceSection
  ]

{- Constants area -}
gWidth, gWidth1 :: Double
gWidth = 900
gWidth1 = gWidth - 2 * gPad1

gHeight1 :: Double
gHeight1 = 30

gPad1 :: Double
gPad1 = 10
{- end [Constants] -}

{- Diagram sections -}
spaceSection :: Diagram B
spaceSection = mkRect gWidth 15 yellow black

titleSection :: Game -> Diagram B
titleSection g = mkTxt gWidth gHeight1 bold $ concat [g ^. title, " (", g ^. dm, ")"]

quoteSection :: Game -> Diagram B
quoteSection g = mkTxt gWidth gHeight1 italic $ concat ["\"", g ^. quote, "\""]

datesSection :: Game -> Diagram B
datesSection g = hcat' (with & catMethod .~ Distrib & sep .~ (gWidth / 3))
  [ mkTxt mw gHeight1 id $ show (g ^. announcementDate)
  , mkTxt mw gHeight1 id $ show (g ^. startDate)
  , mkTxt mw gHeight1 id $ show (g ^. endDate)
  ] # centerXY <> mkRect gWidth gHeight1 green green
  where
    mw = gWidth / 3

playerTableSection :: Game -> Diagram B
playerTableSection g = vcat lnes # bg cyan # frame gPad1
  where
    lnes = header : map pInfo (g ^. players)
    header = mk3 . map (mkTxt mw mh bold) . words $ "Player Role Fate"
    pInfo (p, r, s) = mk3 . map (mkTxt mw mh id) $ [p, show r, show s]
    mk3 = centerXY . hcat
    mw = gWidth1 / 3
    mh = gHeight1 * 2
{- end [Diagram sections] -}

{- local utilities -}
mkTxt :: Double -> Double -> (Diagram B -> Diagram B) -> String -> Diagram B
mkTxt w h style s = textBox 14 s style w h black green

mkRect :: Double -> Double -> Colour Double -> Colour Double -> Diagram B
mkRect w h bgCol lineCol = rect w h # bg bgCol # lc lineCol # lwO 5

textBox :: Double -- font size
  -> String -- text
  -> (Diagram B -> Diagram B)
  -> Double -- width of box
  -> Double -- height of box
  -> Colour Double -- foreground
  -> Colour Double -- background
  -> Diagram B
textBox s t td w h c bc = text t # fc c # fontSizeG s # td <> mkRect w h bc black
