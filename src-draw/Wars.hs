module Wars (doWars) where

import Control.Lens hiding ((#))
import Data.Maybe (catMaybes)
import Data.Colour.SRGB.Linear
import Data.Time
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (view)

import Utils

import Wars.Data
import Wars.Types

doWars :: (Diagram B -> IO ()) -> IO ()
doWars rf = do
  fImgs <- mapM loadImage
    [ "src-draw/res/wars/sign_fed_64x64.png"
    , "src-draw/res/wars/sign_emp_64x64.png"
    , "src-draw/res/wars/sign_uni_64x64.png"
    ]
  today <- fmap utctDay getCurrentTime
  rf $ wars fImgs today

wars :: [Diagram B] -> Day -> Diagram B
wars fi d = vcat $ map (drawEvent fi maxes d) events
  where
    maxes = map _details events ^.. folded . _War . _3 ^. traverse . both

drawEvent :: [Diagram B] -> WarDetails -> Day -> Event -> Diagram B
drawEvent fi w d e
  | isPeace e         = textBox cPeace peaceText
  | isLocalConflict e = textBox cLocal evName
                        ===
                        textBox cLocal intervalText
  | isMajorEvent e    = textBox cSpecial $ mconcat [evName, " ", show $ e ^. startDate]
  | isWar e           = buildWarFrame fi w st en evName (e ^. details)
  | otherwise = error $ "Don't know to draw " ++ show e
  where
    st:en:_ = take 2 . catMaybes $ [e ^? startDate, e ^. endDate, Just d]
    eduration = diffDays en st
    durText = mconcat [show eduration, " days of"]
    intervalText = mconcat ["From ", show st, " to ", show en]
    evName = maybe "" (' ':) $ e ^. name
    peaceText = mconcat [durText, evName, " peace"]

textBox :: Colour Double -> String -> Diagram B
textBox c t = text t # fc black <> frameBox
  where
    frameBox = rect 900 50 # bg c # lc c # alignY (-0.6)

buildWarFrame :: [Diagram B] -> WarDetails -> Day -> Day -> String
  -> EventDetails -> Diagram B
buildWarFrame fi w st en n d = vcat
  [ t 14 n # translate (r2 (350, -12)) <> r 800 50 # translateX 350
  , hcat [i1 <> r 100 100, build f1 c1]
  , hcat [i2 <> r 100 100, build f2 c2]
  , hcat $ map (\tx -> (t 14 tx # translateY (-12))<> r 100 50)
      ["Factions", "Kills", "Structs", "Mission", "Sector",
      "Heroes", "Medals", "Points"]
  , hcat
    [ showTimeslots <> r 300 200
    , scale 2 iW <> r 200 200
    , showScores <> r 300 200
    ] # translateX 100
  ] # centerXY <> rect 900 508 # bg cBG # lc cBG # translateY 3
  where
    r x y = rect x y # style
    r' c x y | y > 0 = rect x y # bg c # lc c # translateY (-44 + y / 2)
    r' _ _ _ = mempty
    style = bg cBG # lc black # lwO 8
    showTimeslots =  display4TextVals
      [ show st, show en, "", show (diffDays en st) ++ " days"]
    showScores = let tab = "        " in display4TextVals
      [ concat [pd pKills,      tab, pd pHeroes]
      , concat [pd pStructures, tab, pd pMedals]
      , concat [pd pMissions,   tab, pd pPoints]
      , concat [pd pSectors,    tab, pd pAll]
      ]
      where
        l@(pKills:pStructures:pMissions:pSectors:pHeroes:pMedals:pPoints:_)
          = map getScore lenses
        getScore f = jaccard (theDetails ^. _1.f) (theDetails ^. _2.f)
        jaccard 0 0 = 1
        jaccard x y = fromIntegral (min x y) / fromIntegral (max x y)
        pAll = sqrt $ sum (map (**2) l) / 7
    display4TextVals = translateY 25 . vcat' (with & sep .~ 30) . map (t 18)
    t fs tx = text tx # bold # fontSizeL fs
    wonFaction = d ^?! winner
    lstFaction = d ^?! loser
    theDetails = d ^?! warDetails
    cW = colorOf wonFaction
    cL = colorOf lstFaction
    iW = fi !! fromEnum wonFaction
    iL = fi !! fromEnum lstFaction
    (f1, c1, i1) = if wonFaction < lstFaction then (_1, cW, iW) else (_2, cL, iL)
    (f2, c2, i2) = if wonFaction < lstFaction then (_2, cL, iL) else (_1, cW, iW)
    build f c = hcat $ map (\x -> buildOne c (theDetails ^. f.x) (w ^. x)) lenses
    lenses = [kills, structures, mission, sector, heroes, medals, points]
    buildOne c v m = buildText v
      <> r' c 86 (86 * fromIntegral v / fromIntegral m)
      <> r 100 100
    buildText v = show v # t 14 # translateY (-15)

pd :: Double -> String
pd x
  | t < 10 = concat [show y, ".0", show t]
  | otherwise = concat [show y, ".", show t]
  where
    y = truncate x :: Integer
    z = fromIntegral y
    t = abs $ truncate (100 * (x - z)) :: Integer

--- main colors
colorOf :: Faction -> Colour Double
colorOf Federation = cFed
colorOf Empire = cEmp
colorOf Union = cUni

cBG, cPeace, cSpecial, cLocal, cFed, cEmp, cUni :: Colour Double
cBG = rgb 0.25 0.25 0.25
cPeace = rgb 0.32 0.59 0.28
cSpecial = rgb 0.80 0.70 0.05
cLocal = rgb 0.50 0.52 0.54
cFed = rgb 0.02 0.23 0.89
cEmp = rgb 0.95 0.00 0.00
cUni = rgb 0.85 0.68 0.00
