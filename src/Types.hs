module Types 
  ( RequestType(..)
  , Request(..)
  , Response(..)
  , RequestQueue
  , validTokens
  ) where

import Control.Concurrent (MVar)
import Data.Time (UTCTime)
import System.Random (Random(..), randomR)

-- | Request data type for modelling an HTTP request
data Request = Request 
  { requestTimestamp :: UTCTime
  , requestType      :: RequestType
  , requestContent   :: String
  , clientToken      :: String
  }
  deriving (Show)

-- | Response data type for modelling an HTTP response
data Response = Response
  { responseTimestamp :: UTCTime
  , responseContent   :: String
  }
  deriving (Show)

-- | RequestQueue data type for modelling the queue of requests received by the server
type RequestQueue = MVar [Request]

-- | Data types for the Extra Features

-- | Extra Feature 1 - Request Type Assignment
-- | Types of requests (GET, POST, DELETE)
data RequestType = GET | POST | DELETE deriving (Show, Enum, Bounded)

-- | Random instance for RequestType
instance Random RequestType where
    random g = 
        let (index, g') = randomR (0, 2) g
            requestTypes = [GET, POST, DELETE]
        in (requestTypes !! index, g')

    randomR (lo, hi) g =
        let (index, g') = randomR (fromEnum lo, fromEnum hi) g
        in (toEnum index, g')

-- | Extra Feature 2 - Authentication Simulation
-- | List of valid tokens
validTokens :: [String]
validTokens = ["46VI4lub7O", "XdBE3lesjh", "AOWu2gMEgI", "q69L0xBG9y", "mdUfIXhQrc"] -- Randomly generated tokens
