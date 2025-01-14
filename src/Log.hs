module Log (logResponse) where

import System.IO (withFile, IOMode(AppendMode), hPutStrLn)
import Types

-- | Function that handles the Log Requests and Responses
logResponse :: Request -> Response -> IO ()
logResponse (Request reqTime reqContent) (Response resTime resContent) =
    withFile "requests.log" AppendMode $ \h -> do
        hPutStrLn h $ "Request: " ++ show reqTime ++ " - " ++ reqContent
        hPutStrLn h $ "Response: " ++ show resTime ++ " - " ++ resContent
