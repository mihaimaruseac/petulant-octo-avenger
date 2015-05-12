module Wars (wars) where

import Control.Lens hiding ((#))
import Data.Maybe (catMaybes)
import Data.Colour.SRGB.Linear
import Data.Time
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (view)

import Wars.Types

wars :: [Diagram B R2] -> Day -> Diagram B R2
wars fi d = vcat $ map (drawEvent fi maxes d) events
  where
    maxes = map _details events ^.. folded . _War . _3 ^. traverse . both

drawEvent :: [Diagram B R2] -> WarDetails -> Day -> Event -> Diagram B R2
drawEvent fi w d e
  | isPeace e         = textBox lightgreen peaceText
  | isLocalConflict e = textBox lightblue evName
                        ===
                        textBox lightblue intervalText
  | isMajorEvent e    = textBox red $ mconcat [evName, " ", show $ e ^. startDate]
  | isWar e           = buildWarFrame fi w st en evName (e ^. details)
  | otherwise = error $ "Don't know to draw " ++ show e
  where
    st:en:_ = take 2 . catMaybes $ [e ^? startDate, e ^. endDate, Just d]
    eduration = diffDays en st
    durText = mconcat [show eduration, " days of"]
    intervalText = mconcat ["From ", show st, " to ", show en]
    evName = maybe "" (' ':) $ e ^. name
    peaceText = mconcat [durText, evName, " peace"]

textBox :: Colour Double -> String -> Diagram B R2
textBox c t = text t # fc black <> frameBox
  where
    frameBox = rect 900 50 # bg c # lc c # alignY (-0.6)

buildWarFrame :: [Diagram B R2] -> WarDetails -> Day -> Day -> String
  -> EventDetails -> Diagram B R2
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
cPeace = rgb 0.32 0.59 0.28 -- TODO: use
cSpecial = undefined -- TODO: use
cLocal = undefined -- TODO: use
cFed = rgb 0.02 0.23 0.89
cEmp = rgb 0.95 0.00 0.00
cUni = rgb 0.85 0.68 0

--- events

events :: [Event]
events =
  [ peace & startDate .~ artemisOpened & endDate ?~ vendettaStart
  , war & details .~ vendetta & name ?~ "Vendetta War"
        & startDate .~ vendettaStart & endDate ?~ vendettaEnd
  , peace & startDate .~ vendettaEnd & endDate ?~ dominionIStart
  , war & details .~ dominionI & name ?~ "Dominion I War"
        & startDate .~ dominionIStart & endDate ?~ dominionIEnd
  , peace & startDate .~ dominionIEnd & endDate ?~ dominionIIStart & name ?~ "hateful"
  , war & details .~ dominionII & name ?~ "Dominion II War"
        & startDate .~ dominionIIStart & endDate ?~ dominionIIEnd
  , peace & startDate .~ dominionIIEnd & endDate ?~ prosperityStart & name ?~ "bountyful"
  , war & details .~ prosperity & name ?~ "Prosperity War"
        & startDate .~ prosperityStart & endDate ?~ prosperityEnd
  , peace & startDate .~ prosperityEnd & endDate ?~ lastWarStart & name ?~ "privateer disturbed"
  , war & details .~ lastWar & name ?~ "The Last War"
        & startDate .~ lastWarStart & endDate ?~ lastWarEnd
  , peace & startDate .~ lastWarEnd & endDate ?~ warsonGate & name ?~ "deceitful"
  , majorEvent & startDate .~ warsonGate & name ?~ "Warsongate"
  , peace & startDate .~ warsonGate & endDate ?~ splitStart & name ?~ "Treaty Enforced"
  , localConflict & startDate .~ splitStart & endDate ?~ splitEnd & name ?~ "Split Conflict (Feds Failed Taking Veedfa... Again)"
  , peace & startDate .~ splitEnd & name ?~ "a strange"
  ]
  where
    artemisOpened   = fromGregorian 2007  6 10
    vendettaStart   = fromGregorian 2008  8  8
    vendettaEnd     = fromGregorian 2008  9 17
    dominionIStart  = fromGregorian 2009  1 18
    dominionIEnd    = fromGregorian 2009  3  4
    dominionIIStart = fromGregorian 2009 12 25
    dominionIIEnd   = fromGregorian 2010  2 12
    prosperityStart = fromGregorian 2010 12 27
    prosperityEnd   = fromGregorian 2011  2 19
    lastWarStart    = fromGregorian 2012  7 15
    lastWarEnd      = fromGregorian 2012  9 10
    warsonGate      = fromGregorian 2013  4  9
    splitStart      = fromGregorian 2014  9 11
    splitEnd        = fromGregorian 2015  1  3
    vendetta   = War Federation Empire (vendettaFed, vendettaEmp)
    dominionI  = War Union Federation (dominionIUni, dominionIFed)
    dominionII = War Federation Union (dominionIIFed, dominionIIUni)
    prosperity = War Federation Empire (prosperityFed, prosperityEmp)
    lastWar    = War Federation Union (lastWarFed, lastWarUni)
    vendettaFed   = Details 315944 185653  15591 114700      0 243 389
    vendettaEmp   = Details 230784 180219  13165  37400      0 210 322
    dominionIUni  = Details 626901 345115  28586 188200  65000 223 463
    dominionIFed  = Details 253492 239480  12812   1200      0 193 306
    dominionIIUni = Details 463370 323590  33180 106600      0 173 373
    dominionIIFed = Details 518503 344271  57432  76800  40000 222 443
    prosperityFed = Details 895138 441423 119915  53800 280000 247 579
    prosperityEmp = Details 390711 311371  39740  39600      0 193 368
    lastWarUni    = Details 480084 207297  46187 226600      0 153 356
    lastWarFed    = Details 495682 265319  72163  78200  80000 178 377
