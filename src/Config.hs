module EnvVars where

import GHC.Generics
import System.Envy

data EnvVars = EnvVars
  { pgHost :: String, -- "PG_HOST"
    pgPort :: Int, -- "PG_PORT"
    httpPort :: Int -- "HTTP_PORT"
  }
  deriving (Generic, Show)

instance FromEnv EnvVars
