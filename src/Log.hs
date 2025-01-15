module Log (logResponse, logUnauthorized, logAuthSuccess, logAuthRetry, logFinalCount) where

import System.IO (withFile, IOMode(AppendMode), hPutStrLn)
import Types

-- | Function to log successful request-response pairs
logResponse :: Request -> Response -> IO ()
logResponse (Request reqTime reqType reqContent token) (Response resTime resContent) =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Authenticated Token: " ++ token
        hPutStrLn h $ "Request: " ++ show reqTime ++ " | Type: " ++ show reqType ++ " | Content: " ++ reqContent
        hPutStrLn h $ "Response: " ++ show resTime ++ " | Content: " ++ resContent
        hPutStrLn h $ replicate 50 '-'

-- | Function to log unauthorized requests
logUnauthorized :: Request -> IO ()
logUnauthorized (Request reqTime reqType reqContent token) =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Unauthorized Request: " ++ show reqTime ++ " | Type: " ++ show reqType ++ " | Content: " ++ reqContent ++ " | Token: " ++ token
        hPutStrLn h $ replicate 50 '-'

-- | Function to log successful authentication
logAuthSuccess :: String -> IO ()
logAuthSuccess token =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Authentication Successful: Token - " ++ token
        hPutStrLn h $ replicate 50 '-'

-- | Function to log retries for failed authentication
logAuthRetry :: Request -> IO ()
logAuthRetry (Request reqTime reqType reqContent token) =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Authentication Failed (Retry): Token - " ++ token
        hPutStrLn h $ "Request Requeued: " ++ show reqTime ++ " | Type: " ++ show reqType ++ " | Content: " ++ reqContent
        hPutStrLn h $ replicate 50 '-'

-- | Function to log the final request count
logFinalCount :: Int -> IO ()
logFinalCount count =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Final Summary: Total Requests Processed = " ++ show count
