{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import App (App, Environment)
import App qualified
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text
import Data.Time (Day (..))
import Database.PostgreSQL.Simple (FromRow, Only (..), query, query_)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( defaultManagerSettings,
    newManager,
  )
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Client

type API =
  "locations" :> Get '[JSON] [City]
    :<|> "location" :> Capture "city" Text :> "weather" :> Get '[JSON] WeatherEntry

newtype State = State Text deriving (Show, Eq, Ord, Generic)

instance FromField State where
  fromField f mdata = State <$> fromField f mdata

instance FromJSON State

instance ToJSON State

newtype Temperature = Temperature Int deriving (Show, Eq, Ord, Generic, FromField)

instance FromJSON Temperature

instance ToJSON Temperature

data City = City
  { cityName :: Text,
    state :: State,
    latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Eq, Ord, Generic)

instance FromRow City where
  fromRow = City <$> field <*> (State <$> field) <*> field <*> field

instance FromJSON City

instance ToJSON City

data WeatherEntry = WeatherEntry
  { city :: City,
    date :: Day,
    temperature :: Temperature
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherEntry

instance ToJSON WeatherEntry

-- server :: Server API
-- server = (liftIO .locations) :<|> (liftIO . weatherFor)

-- List of available cities that have weather data.
-- It can be a hard coded list or cities from a state.
-- From a single state?
locations :: App [City]
locations = do
  conn <- asks App.connection
  liftIO $ query_ conn "SELECT name, state, latitude, longitude FROM cities"

weatherFor :: Text -> App WeatherEntry
weatherFor cityName = do
  conn <- asks App.connection
  weather <-
    liftIO $
      query
        conn
        [sql| SELECT name, state, latitude, longitude, date, temperature FROM weather
            join cities on weather.city_id = cities.id
            WHERE city = ? and date = CURRENT_DATE |]
        (Only cityName)
  case weather of
    [] -> error "No weather data for today" -- TODO get the current weather, cache it, and return it
    [(cityName', state', latitude', longitude', date', temperature')] ->
      pure $ WeatherEntry (City state' cityName' latitude' longitude') date' temperature'
    _ -> error "Multiple weather entries for today" -- TODO log this error but still return

-- Reference for boilerplate: https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
api :: Proxy API
api = Proxy

nt :: Environment -> App a -> Handler a
nt env a = case App.unApp a of
  ReaderT f -> liftIO $ f env

app :: Environment -> Application
app env = serve api $ hoistServer api (nt env) server

server :: ServerT API App
server = locations :<|> weatherFor

main :: IO ()
main = do
  putStrLn "Starting server..."
  environment <- App.loadEnvironment
  let port = App.port environment
  run port $ app environment
