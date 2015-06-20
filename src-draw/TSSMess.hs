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
doTssMess = undefined
