module Types where

import Control.Concurrent (MVar)
import Data.Time (UTCTime)

-- | Request data type for modelling an HTTP request
data Request = Request 
  { reqTimestamp :: UTCTime
  , reqContent   :: String
  }
  deriving (Show)

-- | Response Data type for modelling an HTTP response
data Response = Response
  { resTimestamp :: UTCTime
  , resContent   :: String
  }
  deriving (Show)

-- | RequestQueue data type for modelling the queue of requests received by the server
type RequestQueue = MVar [Request]