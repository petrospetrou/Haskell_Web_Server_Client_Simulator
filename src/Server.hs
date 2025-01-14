module Server (server) where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad (when)
import Types
import Log (logResponse)
import Data.Time (getCurrentTime)

-- | Function that processes the requests and generates a response
processRequest :: Request -> IO Response
processRequest (Request requestTime content) = do
    responseTime <- getCurrentTime
    let responseContent = "Response to: " ++ content
    return $ Response responseTime responseContent

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
                    response <- processRequest req
                    logResponse req response
                    modifyMVar_ counter (\c -> return (c + 1))
                    return rest
            processRequests
