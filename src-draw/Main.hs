-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
