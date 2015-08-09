module TSSMess (doTssMess) where

--import Control.Lens
--import Data.Text
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (view)

import Args
import Utils

import TSSMess.Data
import TSSMess.Types

doTssMess :: TSSMessCommands -> (Diagram B R2 -> IO ()) -> IO ()
doTssMess m rf = do
  fImgs <- mapM loadImage
    [ "src-draw/res/wars/sign_fed_64x64.png"
    , "src-draw/res/wars/sign_emp_64x64.png"
    , "src-draw/res/wars/sign_uni_64x64.png"
    ]
  rf undefined
