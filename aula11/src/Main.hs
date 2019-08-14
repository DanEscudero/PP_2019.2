{-# LANGUAGE OverloadedStrings #-}

module Main where

import Formatting
import System.Clock
import Control.DeepSeq
import Formatting.Clock
import System.Environment
import Data.List.Split (chunksOf)

parseFile :: String -> [[Double]]
parseFile file = map (map read . words) (lines file)

main :: IO ()
main = do
  print "hello world"
  let fname = "./src/data.csv"
  [sk, sit, schunks] <- getArgs

  fileTrain <- readFile fname
  let k =       read sk :: Int
      it =      read sit :: Int
      nchunks = read schunks :: Int
      train =   parseFile fileTrain

      -- chunks = force $ chunksOf nchunks train
      -- clusters = take k train

  print train
  
  start <- getTime Monotonic

  stop <- getTime Monotonic
  fprint (timeSpecs % "\n") start stop
  return ()