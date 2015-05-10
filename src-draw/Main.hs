import Data.Time (getCurrentTime, utctDay)
import Data.Monoid (mempty)
import Diagrams.Backend.CmdLine (mainRender)
import Diagrams.Backend.Rasterific.CmdLine (B)
import Diagrams.Prelude (Diagram, R2)
import Diagrams.TwoD.Image (loadImageEmb, image)

import Args
import Demos
import TSSMess
import Wars

main :: IO ()
main = do
  args <- parseArgs
  case args of
    -- demos first
    Demo n o     -> mainRender o $ selectDemo (Tutorial n)
    ADemo n o    -> mainRender o $ selectDemo (Arrow' n)
    TDemo n o    -> mainRender o $ selectDemo (Trails n)
    VDemo n o    -> mainRender o $ selectDemo (Vector n)
    -- special demos
    Arrow o      -> mainRender o demoArrow
    Tournament o -> mainRender o demoTournament
    -- other
    TSSMess      -> tssMess
    Wars o       -> doWars o
    _            -> print args

doWars :: DO -> IO ()
doWars o = do
  fImgs <- mapM loadImage
    [ "src-draw/res/wars/sign_fed_64x64.png"
    , "src-draw/res/wars/sign_emp_64x64.png"
    , "src-draw/res/wars/sign_uni_64x64.png"
    ]
  today <- fmap utctDay getCurrentTime
  mainRender o $ wars fImgs today

loadImage :: FilePath -> IO (Diagram B R2)
loadImage path = do
  img <- loadImageEmb path
  case img of
    Left _  -> putStrLn ("Invalid image path " ++ path) >> return mempty
    Right i -> return $ image i
