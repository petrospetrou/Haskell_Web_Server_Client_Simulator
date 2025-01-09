module Types where

import Control.Concurrent (MVar)
import Data.Time (UTCTime)

-- | Data type for an HTTP-like request
data Request = Request 
  { reqTimestamp :: UTCTime
  , reqContent   :: String
  }
  deriving (Show)

-- | Data type for an HTTP-like response
data Response = Response
  { resTimestamp :: UTCTime
  , resContent   :: String
  }
  deriving (Show)
