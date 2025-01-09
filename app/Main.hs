{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, replicateM_)
import Data.Time (getCurrentTime, UTCTime)
import System.Random (randomRIO)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn)

-- Data Types for Request and Response
data Request = Request { reqTimestamp :: UTCTime, reqContent :: String } deriving (Show)
data Response = Response { resTimestamp :: UTCTime, resContent :: String } deriving (Show)

type RequestQueue = MVar [Request]

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
server :: RequestQueue -> IO ()
server queue = do
    replicateM_ 100 $ do
        requests <- readMVar queue
        case requests of
            [] -> threadDelay 100000  -- Wait if no requests
            (req:rest) -> do
                modifyMVar_ queue (const $ return rest)
                response <- processRequest req
                logResponse req response

-- Client function
client :: Int -> RequestQueue -> IO ()
client clientId queue = do
    replicateM_ 10 $ do
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
    forkIO $ server queue
    forM_ [1..10] $ \clientId -> forkIO $ client clientId queue
    threadDelay 5000000  -- Wait for all threads to complete before exiting
