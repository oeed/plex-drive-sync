module Main where

import Lib
import Config

main :: IO ()
main = do
  config <- readConfig
