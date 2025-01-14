module Types where

import Control.Concurrent (MVar)
import Data.Time (UTCTime)

-- | Request data type for modelling an HTTP request
data Request = Request 
  { requestTimestamp :: UTCTime
  , requestContent   :: String
  }
  deriving (Show)

-- | Response Data type for modelling an HTTP response
data Response = Response
  { responseTimestamp :: UTCTime
  , responseContent   :: String
  }
  deriving (Show)

-- | RequestQueue data type for modelling the queue of requests received by the server
type RequestQueue = MVar [Request]