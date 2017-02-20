{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Demos (Demo(..), selectDemo, demoTournament, demoArrow) where

import Data.List.Split (chunksOf)

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

data Demo
  = Arrow' Int
  | Trails Int
  | Tutorial Int
  | Vector Int

selectDemo :: Demo -> Diagram B
selectDemo (Tutorial n) = doSelectDemo demos n
selectDemo (Trails n) = doSelectDemo trailsDemos n
selectDemo (Vector n) = doSelectDemo vectorDemos n
selectDemo (Arrow' n) = doSelectDemo arrowsDemos n

demoTournament :: Diagram B
demoTournament = tournament 16

doSelectDemo :: [Diagram B] -> Int -> Diagram B
doSelectDemo ds n
  | 0 < n && n <= l = ds !! (n - 1)
  | otherwise = error $ concat ["Demo not defined (not in {1, 2.. ", show l, "})"]
  where
    l = length ds

demos :: [Diagram B]
demos =
  [ demoCircle
  , demoCircleStyled
  , demoTwoCircles
  , demoAtop
  , demoOrigin
  , demoOrigin2
  , demoSide
  , demoSideOrigin
  , demoBeside
  , demoHcat
  , demoEllipse
  , demoSnug
  , demoRotate
  , demoAlign
  ]

demoCircle :: Diagram B
demoCircle = circle 1

demoCircleStyled :: Diagram B
demoCircleStyled = circle 1 # fc blue # lw veryThick # lc purple # dashingG [0.2, 0.05] 0

demoAtop :: Diagram B
demoAtop = square 1 # fc aqua `atop` circle 1

-- show origins
demoOrigin :: Diagram B
demoOrigin = circle 1 # showOrigin

demoOrigin2 :: Diagram B
demoOrigin2 = demoAtop # showOrigin

-- stack horizontally
demoTwoCircles :: Diagram B
demoTwoCircles = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none

-- stack vertically
demoSide :: Diagram B
demoSide = circle 1 === square 2

-- origin is origin of first element
demoSideOrigin :: Diagram B
demoSideOrigin = demoSide # showOrigin

-- beside uses vector displacements (r2)
demoBeside :: Diagram B
demoBeside = beside (r2 (1,1)) (circle 1) (square 2)

-- horizontal concat (hcat is concat with |||) and strutX for spaces
demoHcat :: Diagram B
demoHcat = hcat [hcat [demoBeside, demoBeside], strutX 1, demoBeside ||| demoBeside]

-- ellipses. Since positioning is based on a separation line perp to the
-- center lines there is a gap in between
demoEllipse :: Diagram B
demoEllipse = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell ||| ell

-- solve the above gap issue
demoSnug :: Diagram B
demoSnug = let ell = circle 1 # scaleX 0.5 # rotateBy (1/6) in ell # snugR <> ell # snugL

-- demo rotate by fraction of circle: rotate by 2pi/3 (120 degrees)
demoRotate :: Diagram B
demoRotate = square 1 # rotateBy (1/3)

-- demo align
demoAlign :: Diagram B
demoAlign = hrule (2 * sum sizes) === circles # centerX
  where circles = hcat . map alignB . zipWith scale sizes
                $ repeat (circle 1)
        sizes   = [2,5,4,7,1,3]

tournament :: Int -> Diagram B
tournament n = atPoints (regPoly n 1) (map (node n) [1..]) #
  applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]
  where
    arrowOpts = with & gaps       .~ small
                     & headLength .~ global 0.2

-- remember that <> is `atop`
node :: Int -> Int -> Diagram B
node m n
   = text (show n) # fontSizeN (0.5 / fromIntegral m) # fc white # translate (r2 (-0.01, -0.18))
  <> circle 0.2 # fc green # named n

vectorDemos :: [Diagram B]
vectorDemos =
  [ demoChainSaw
  , demoSemicircle
  , demoSpokes
  , vTriangle (unitX # rotateBy (3/7)) (unitX # rotateBy (1/8))
  , vAddRule (unitX # rotateBy (3/7)) (unitX # rotateBy (1/8))
  , demoPoints
  ]

-- fromOffsets takes a list of vectors to draw
-- r2 construct vectors from pairs
demoChainSaw :: Diagram B
demoChainSaw = fromOffsets . map (curry r2 1 . s) $ [(1::Int)..10]
  where
    s x = if odd x then 1 else -1

-- fromDirection builds a vector on a direction (rad, turn or deg, use (@@) to
-- specify)
-- Also showcase fromDirection and scalar-vector multiplication
demoSemicircle :: Diagram B
demoSemicircle = undefined {- TODO: fix errors
mconcat . map build $ [-3, -2.. 3]
  where
    build = flip translate c . (5 *^) . rotate . (@@ turn) . (* 30)
    c = circle 1 # fc blue
    -}

-- list of lists: inner for spikes, outer for diagrams
demoSpokes :: Diagram B
demoSpokes = undefined {- TODO: fix error
mconcat $ map buildOne [1..3]
  where
    s = 10
    buildOne x = rotate (angleDir (x/3 @@ turn)) $ buildStar x
    buildStar x = mconcat . map (f x) $ [1..s]
    f x y = fromOffsets [x *^ rotate (y/s @@ turn)]
    -}

-- vTriangle builds a triangle given two sides
vTriangle :: V2 Double -> V2 Double -> Diagram B
vTriangle a b = fromOffsets [a, b - a, -b]

-- vAddRule shows the vector addition rule
vAddRule :: V2 Double -> V2 Double -> Diagram B
vAddRule a b = mconcat
  [ fromOffsets [a + b] # vSum
  , mconcat $ build a b
  , mconcat $ build b a
  ]
  where
    build x y = [fromOffsets [x] # v, translate y $ fromOffsets [x] # vHlp]
    v         = lc blue
    vSum      = lc red
    vHlp      = lc purple # dashingG [0.2, 0.05] 0

-- use ^& but could also use p2
demoPoints :: Diagram B
demoPoints = position [(p, c p) | x <- l, y <- l, let p = x ^& y]
  where
    s = 15
    l = [-s .. s]
    c p = circle 1 # fc (if distance p origin < s then yellow else purple)

trailsDemos :: [Diagram B]
trailsDemos =
  [ demoBasicTrailFromOffsets
  , demoFromVertices
  , demoOnLineSegments
  , demoOnLineSegments2
  , demoKoch51
  , demoKoch52
  , demoKoch53
  , demoBlob
  , demoGrass
  , demoExplode
  , demoSquare
  , demoStar
  , demoFill
  ]

-- or map v2
demoBasicTrailFromOffsets :: Diagram B
demoBasicTrailFromOffsets = fromOffsets [1 ^& 0, 0 ^& 2, 2 ^& 0]

-- or map p2
demoFromVertices :: Diagram B
demoFromVertices = mconcat . map (`translateX` b) $ [1..4]
  where
    b = fromVertices [0 ^& 0, 0 ^& 1, 1 ^& 0]

-- strokeLine to convert a trail to diagram
demoOnLineSegments :: Diagram B
demoOnLineSegments = strokeLine auxDemoTrailsLine

-- no need to translate to match, mappend (<>) starts from where it stopped
demoOnLineSegments2 :: Diagram B
demoOnLineSegments2 = strokeLine $ auxDemoTrailsLine <> auxDemoTrailsLine

demoKoch51 :: Diagram B
demoKoch51 = koch1 7 # strokeLine

demoKoch52 :: Diagram B
demoKoch52 = strokeLine $ koch2 5

-- compare with demoKoch51
demoKoch53 :: Diagram B
demoKoch53 = koch1 7 # glueLine # strokeLoop # fc green

auxDemoTrailsLine :: Trail' Line V2 Double
auxDemoTrailsLine = onLineSegments (take 4) $ pentagon 1

koch1 :: Int -> Trail' Line V2 Double
koch1 s = mconcat . iterateN s f $ auxDemoTrailsLine
  where
    f = rotate ((1/fromIntegral s) @@ turn)

koch2 :: Int -> Trail' Line V2 Double
koch2 0 = mempty
koch2 n = mconcat
  [ l # rotateBy (-1/3) # reverseLine
  , fromOffsets [unitX] # rotateBy (1/6)
  , l
  , fromOffsets [unitX] # rotateBy (-1/6)
  , l # rotateBy (1/3) # reverseLine
  ]
  where
    l = koch2 $ n - 1

demoBlob :: Diagram B
demoBlob = blob 19 # glueLine # strokeLoop # fc blue

blob :: Int -> Trail' Line V2 Double
blob n = foldl andThen mempty $ replicate n arm
  where
    arm = seg `andThen` cap `andThen` seg `andThen` cup
    cap = arc xDir (1/2 @@ turn)
    cup = arc xDir ((fromIntegral n-2)/(2* fromIntegral n) @@ turn) # reflectX
    seg = fromOffsets [unitX]

andThen :: Trail' Line  V2 Double-> Trail' Line  V2 Double-> Trail' Line V2 Double
andThen t1 t2 = t1 <> t2 # rotate (signedAngleBetweenDirs d1 d2)
  where
    d1 = direction (tangentAtEnd t1)
    d2 = direction (tangentAtStart t2)

demoGrass :: Diagram B
demoGrass = grass 31 # closeLine # strokeLoop # fc green

grass :: Int -> Trail' Line V2 Double
grass n = mconcat [fromOffsets [unitY], gl, fromOffsets [unit_Y]]
  where
    gl = mconcat  . replicate n . fromOffsets $ [1 ^& h, 1 ^& (-h)]
    h = 3

-- explode trail
-- observe pad, strokeLocTrail and mapLoc
demoExplode :: Diagram B
demoExplode = heptagon 1 # explodeTrail # map f # mconcat # pad 1.1
  where
    f = strokeLocTrail . mapLoc (rotateBy (1/20))

-- at, centerXY
demoSquare :: Diagram B
demoSquare = squareTrail # explodeTrail # zipWith lc (cycle [red, blue]) #
  mconcat # centerXY # pad 1.1
  where
    squareTrail = iterateN 4 (rotateBy (1/4)) (fromOffsets $
      replicate 4 unitX) # mconcat # wrapLine # (`at` origin)

-- star, pathTrails
demoStar :: Diagram B
demoStar = mkStar 5 # pathTrails # map strokeLocTrail #
  zipWith lc [red,orange,yellow,blue,green,purple] # mconcat
  where
    mkStar n = star (StarSkip n) (regPoly (6*n) 1)

-- trailVertices, fillRule
-- need a stroke
demoFill :: Diagram B
demoFill = undefined {- TODO: solve error
(circles <> circle 3) # stroke # fc blue # fillRule EvenOdd
  where
    circles = atPoints (hexagon 2) (repeat $ circle 1) <> circle 1
    -}

demoArrow :: Diagram B
demoArrow = connect'        arrow1 "1" "2"
          . connect'        arrow2 "4" "3"
          . connect'        arrow3 "1" "6"
          . connectOutside' arrow4 "4" "8"
          . connect'        arrow5 "9" "5"
          . connectOutside' arrow6 "8" "9"
          . connectOutside' arrow7 "8" "7"
          $ cGrid
  where
    -- The arrows
    arrow1 = with & arrowHead  .~ dart   & headLength .~ veryLarge
                  & arrowTail  .~ quill  & shaftStyle %~ lw thick . lc black
                  & arrowShaft .~ shaft0 & headStyle  %~ fc blue
                  & tailStyle  %~ fc red & tailLength .~ large

    arrow2 = with & arrowHead  .~ dart    & headLength .~ large
                  & arrowTail  .~ dart'   & tailLength .~ large
                  & shaftStyle %~ lw thin & arrowShaft .~ shaft1

    arrow3 = with & arrowHead  .~ thorn         & headLength .~ veryLarge
                  & arrowShaft .~ quartercircle & arrowTail  .~ noTail
                  & gaps .~ normal

    arrow4 = with & arrowHead  .~ dart    & arrowTail  .~ dart'
                  & headLength .~ large   & tailLength .~ large
                  & arrowShaft .~ shaft2  & headStyle  %~ fc teal
                  & tailStyle  %~ fc teal & shaftStyle %~ lw thick . lc teal

    arrow5 = with & arrowTail  .~ spike'     & tailLength .~ veryLarge
                  & arrowShaft .~ semicircle & arrowHead  .~ spike
                  & headLength .~ veryLarge  & headStyle  %~ fc darkorange
                  & tailStyle  %~ fc darkorange
                  & shaftStyle %~ lw veryThick . lc navy

    arrow6 = with & arrowHead  .~ tri & arrowTail .~ tri' & headLength .~ large
                  & headStyle  %~ fc black . opacity 0.5
                  & tailStyle  %~ fc black . opacity 0.5
                  & shaftStyle %~ dashingN [0.01,0.02,0.03,0.01] 0

    arrow7 = arrow6 & arrowHead .~ tri & arrowTail .~ tri'
    -- helpers
    -- Create a 3 x 3 grid of circles named "1" to "9"
    c = circle 1.5 # fc lightgray # lw none # showOrigin
    cs = [c # named (show x) | x <- [1::Int ..9]]
    cGrid = vcat' (with & sep .~ 4)
          . map (hcat' $ with & sep .~ 12)
          . chunksOf 3 $ cs

    -- For the Shafts.
    semicircle = arc (angleDir (5/12 @@ turn)) (6/12 @@ turn)
    quartercircle = arc (angleDir (1/4 @@ turn)) (1/2 @@ turn)

    parab = bezier3 (1 ^& 1) (1 ^& 1) (0 ^& 2)
    parab' = reflectX parab
    seg = straight unitX

    shaft0 = trailFromSegments [parab, seg, parab', seg, parab]
    shaft1 = cubicSpline False (trailVertices (shaft0 `at` origin))
    shaft2 = cubicSpline False (map p2 [(0,0), (1,0), (0.8, 0.2),(2, 0.2)])

arrowsDemos :: [Diagram B]
arrowsDemos =
  [ demoCircularArrow
  , demoSquareArrowShafts
  , demoVectorField
  , demoTorusConnectPerimeter
  ]

demoCircularArrow :: Diagram B
demoCircularArrow = mconcat
  [ circle 1
  , position . zip [pA, pB] $ repeat spot
  , arrowBetween pA pB
  ]
  where
    pA = p2 (1, 0) # rotateBy (1/8)
    pB = p2 (-1, 0)
    spot = circle 0.02 # lw none # fc blue

-- observe reverseTrail to flip arrow's curvature
demoSquareArrowShafts :: Diagram B
demoSquareArrowShafts = mconcat
  [ square 4
  , arrowBetween' (with & arrowTail .~ spike' & arrowHead .~ noHead) pA pB
  , arrowBetween' (a sDown) pA pB
  , arrowBetween' (a sUp) pA pB
  ] # frame 0.2
  where
    [pA, pB] = zipWith (^&) [1, 3] [1, 3]
    a s = with & arrowTail .~ tri' & arrowHead .~ tri & arrowShaft .~ s
    sDown = arc xDir ((1/8) @@ turn)
    sUp = sDown # reverseTrail

-- arrowAt
demoVectorField :: Diagram B
demoVectorField = undefined {- TODO: fix error
position . map (p2 &&& arrowAtPoint) $ locs
  where
    vectorField (x, y) = r2 (sin (y - 1), sin (x + 1))
    locs = let xs = [-3, -2.7 .. 3] in [(x, y) | x <- xs, y <- xs]
    arrowAtPoint p = arrowAt' opts (p2 p) (sL *^ vf) # alignTL
      where
        vf   = vectorField p
        m    = norm p
        -- Head size is a function of the length of the vector
        -- as are tail size and shaft length.
        hs   = 0.04 * m
        sW   = 0.015 * m
        sL   = 0.01 + 0.1 * m
        opts = with & arrowHead .~ tri & headLength .~ global hs & shaftStyle %~ lwG sW
        -}

demoTorusConnectPerimeter :: Diagram B
demoTorusConnectPerimeter = foldr f circles [0..12]
  where
    f x = connectPerim "in" "out" ((x/12) @@ turn) ((x/12) @@ turn)
    circles = mconcat
      [ circle 1 # fc orange # named "in"
      , circle 5 # fc yellow # named "out"
      ]
