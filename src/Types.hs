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

-- | Types of requests (GET, POST, DELETE)
data RequestType = GET | POST | DELETE deriving (Show, Enum, Bounded)

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

-- | List of valid tokens
validTokens :: [String]
validTokens = ["token123", "token456", "token789", "tokenABC", "tokenDEF"]

-- | Random instance for RequestType
instance Random RequestType where
    random g = 
        let (index, g') = randomR (0, 2) g
            requestTypes = [GET, POST, DELETE]
        in (requestTypes !! index, g')

    randomR (lo, hi) g =
        let (index, g') = randomR (fromEnum lo, fromEnum hi) g
        in (toEnum index, g')
