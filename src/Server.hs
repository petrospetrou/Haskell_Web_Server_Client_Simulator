module Server (server) where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad (when)
import System.Random (randomRIO)
import Types
import Log (logResponse, logUnauthorized, logAuthSuccess, logAuthRetry, logFinalCount)
import Data.Time (getCurrentTime)

-- | Function that processes the requests and generates a response
processRequest :: Request -> IO Bool
processRequest (Request reqTime reqType content token) = do
    authResult <- randomRIO (1 :: Int, 100 :: Int)  -- Random number between 1 and 100
    responseTime <- getCurrentTime

    if authResult > 20 && token `elem` validTokens
        then do
            logAuthSuccess token  -- Log successful authentication
            let responseText = case reqType of
                    GET    -> "GET Response to: " ++ content
                    POST   -> "POST Response to: " ++ content
                    DELETE -> "DELETE Response: " ++ content ++ " has been deleted"
            logResponse (Request reqTime reqType content token) (Response responseTime responseText)
            return True
        else do
            logAuthRetry (Request reqTime reqType content token)  -- Log retry
            return False

-- | Function that simulates the Web Server
server :: RequestQueue -> MVar Int -> IO ()
server queue counter = processRequests
  where
    processRequests = do
        count <- readMVar counter
        when (count < 100) $ do
            modifyMVar_ queue $ \reqs -> case reqs of
                [] -> return reqs
                (req:rest) -> do
                    success <- processRequest req
                    if success
                        then modifyMVar_ counter (\c -> return (c + 1))
                        else return ()
                    return (if success then rest else rest ++ [req])
            processRequests
        -- Log the final count once 100 requests are processed
        when (count == 100) $
            logFinalCount count
