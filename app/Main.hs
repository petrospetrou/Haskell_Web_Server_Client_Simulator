module Main (main) where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, putMVar, takeMVar)
import Control.Monad (forever, replicateM_)
import Data.Time (getCurrentTime, UTCTime)
import System.Random (randomRIO)
import System.IO (appendFile)

import Lib
import Types

main :: IO ()
main = someFunc
