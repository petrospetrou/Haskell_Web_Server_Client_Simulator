module Client (client) where

import Control.Concurrent (threadDelay, MVar, modifyMVar_, readMVar)
import Control.Monad (replicateM_, when)
import System.Random (randomRIO)
import Types
import Data.Time (getCurrentTime)

createRequest :: Int -> IO Request
createRequest clientId = do
    timestamp <- getCurrentTime
    let content = "Client " ++ show clientId ++ " says hello!"
    return $ Request timestamp content

client :: Int -> RequestQueue -> MVar Int -> IO ()
client clientId queue counter = do
    replicateM_ 10 $ do
        count <- readMVar counter
        when (count < 100) $ do
            delay <- randomRIO (100000, 500000)
            threadDelay delay
            request <- createRequest clientId
            modifyMVar_ queue (\reqs -> return (reqs ++ [request]))
