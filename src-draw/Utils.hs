module Utils (loadImage) where

import Data.Monoid (mempty)
import Diagrams.Backend.Rasterific.CmdLine (B)
import Diagrams.Prelude (Diagram, R2)
import Diagrams.TwoD.Image (loadImageEmb, image)

loadImage :: FilePath -> IO (Diagram B R2)
loadImage path = do
  img <- loadImageEmb path
  case img of
    Left _  -> putStrLn ("Invalid image path " ++ path) >> return mempty
    Right i -> return $ image i
