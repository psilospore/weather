module EnvVars where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import System.Envy

data EnvVars = EnvVars
  { pgConnString :: ByteString, -- "PG_CONN_STRING"
    httpPort :: Int, -- "HTTP_PORT"
    openWeatherApiKey :: Text -- "OPEN_WEATHER_API_KEY"
  }
  deriving (Generic, Show)

instance FromEnv EnvVars
