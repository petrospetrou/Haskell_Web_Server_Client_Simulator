module Main where

import Control.Concurrent (forkIO, newMVar, threadDelay, MVar, readMVar)
import Control.Monad (when, forM_)
import System.IO (withFile, IOMode(WriteMode))

import Types
import Server (server)
import Client (client)

main :: IO ()
main = do
    queue <- newMVar []
    counter <- newMVar 0
    withFile "requests.log" WriteMode $ const (return ())  -- Clear the log file at start
    forkIO $ server queue counter
    forM_ [1..10] $ \clientId -> forkIO $ client clientId queue counter
    waitForCompletion counter

waitForCompletion :: MVar Int -> IO ()
waitForCompletion counter = do
    count <- readMVar counter
    when (count < 100) $ do
        threadDelay 100000  -- Check every 100ms
        waitForCompletion counter
