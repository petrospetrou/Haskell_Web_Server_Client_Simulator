{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, replicateM_, when)
import Data.Time (getCurrentTime, UTCTime)
import System.Random (randomRIO)
import System.IO (withFile, IOMode(AppendMode, WriteMode), hPutStrLn)

import Types

-- Function to create a random request
createRequest :: Int -> IO Request
createRequest clientId = do
    timestamp <- getCurrentTime
    let content = "Client " ++ show clientId ++ " says hello!"
    return $ Request timestamp content

-- Function to process a request and generate a response
processRequest :: Request -> IO Response
processRequest (Request reqTime content) = do
    resTime <- getCurrentTime
    let responseContent = "Response to: " ++ content
    return $ Response resTime responseContent

-- Server function
server :: RequestQueue -> MVar Int -> IO ()
server queue counter = do
    let processRequests = do
            count <- readMVar counter
            when (count < 100) $ do
                modifyMVar_ queue $ \reqs -> case reqs of
                    [] -> return reqs
                    (req:rest) -> do
                        response <- processRequest req
                        logResponse req response
                        modifyMVar_ counter (\c -> return (c + 1))
                        return rest
                processRequests
    processRequests

-- Client function
client :: Int -> RequestQueue -> MVar Int -> IO ()
client clientId queue counter = do
    replicateM_ 10 $ do
        count <- readMVar counter
        when (count < 100) $ do
            delay <- randomRIO (100000, 500000)
            threadDelay delay
            request <- createRequest clientId
            modifyMVar_ queue (\reqs -> return (reqs ++ [request]))

-- Log requests and responses
logResponse :: Request -> Response -> IO ()
logResponse (Request reqTime reqContent) (Response resTime resContent) =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Request: " ++ show reqTime ++ " - " ++ reqContent
        hPutStrLn h $ "Response: " ++ show resTime ++ " - " ++ resContent

-- Main program
main :: IO ()
main = do
    queue <- newMVar []
    counter <- newMVar 0
    withFile "requests.log" WriteMode $ const (return ())  -- Clear the log file at start
    forkIO $ server queue counter
    forM_ [1..10] $ \clientId -> forkIO $ client clientId queue counter
    -- Dynamically wait for exactly 100 requests to complete
    let waitForCompletion = do
            count <- readMVar counter
            when (count < 100) $ do
                threadDelay 100000  -- Check every 100ms
                waitForCompletion
    waitForCompletion
