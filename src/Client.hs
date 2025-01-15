module Client (client) where

import Control.Concurrent (threadDelay, MVar, modifyMVar_, readMVar)
import Control.Monad (replicateM_, when)
import System.Random (randomRIO)
import Types
import Data.Time (getCurrentTime)

-- | Function to create a random request
createRequest :: Int -> IO Request
createRequest clientId = do
    timestamp <- getCurrentTime
    requestType <- randomRIO (GET, DELETE)  -- Randomly select a request type
    let content = "Client " ++ show clientId ++ " says he/she loves Functional Programming!"
    token <- randomRIO (0, length validTokens - 1)
    return $ Request timestamp requestType content (validTokens !! token)

-- | Function that simulates the Client
client :: Int -> RequestQueue -> MVar Int -> IO ()
client clientId queue counter = do
    replicateM_ 10 $ do
        count <- readMVar counter
        when (count < 100) $ do
            delay <- randomRIO (100000, 500000)  -- Random delay between requests
            threadDelay delay
            request <- createRequest clientId
            modifyMVar_ queue (\reqs -> return (reqs ++ [request]))
