{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import App (App, Environment)
import App qualified
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks, liftIO, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text
import Data.Time (Day (..), UTCTime (utctDay))
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow, execute, query, query_)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant
import WeatherApi (WeatherResponse (WeatherResponse))
import WeatherApi qualified

type API =
  "locations" :> Get '[JSON] [City]
    :<|> "location" :> Capture "cityId" Int :> "weather" :> Get '[JSON] WeatherEntry

newtype State = State Text deriving (Show, Eq, Ord, Generic)

instance FromField State where
  fromField f mdata = State <$> fromField f mdata

instance ToField State where
  toField (State state) = toField state

instance FromJSON State

instance ToJSON State

data City = City
  { cityId :: Int,
    cityName :: Text,
    state :: State,
    latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Eq, Ord, Generic)

instance FromRow City where
  fromRow = City <$> field <*> field <*> (State <$> field) <*> field <*> field

instance ToRow City where
  toRow (City cityId cityName state latitude longitude) =
    [toField cityId, toField cityName, toField state, toField latitude, toField longitude]

instance FromJSON City

instance ToJSON City

data WeatherEntry = WeatherEntry
  { city :: City,
    date :: Day,
    temperature :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherEntry

instance ToJSON WeatherEntry

-- List of available cities that have weather data.
-- It can be a hard coded list or cities from a state.
-- From a single state?
locations :: App [City]
locations = do
  conn <- asks App.connection
  liftIO $ query_ conn "SELECT id, name, state, latitude, longitude FROM cities"

weatherFor :: Int -> App WeatherEntry
weatherFor cityId = do
  conn <- asks App.connection
  [city] <- liftIO $ query conn "select * from cities where id = ?" (Only cityId)
  -- TODO withTransaction otherwise we can have race conditions that lead to duplicate entries
  cachedWeather <-
    liftIO $
      query
        conn
        [sql| SELECT cities.id, name, state, latitude, longitude, date, temperature FROM weather
            join cities on weather.city_id = cities.id
            WHERE city_id = ? and date = CURRENT_DATE |]
        (Only cityId)
  case cachedWeather of
    [(cityId', cityName', state', latitude', longitude', date', temperature')] ->
      pure $ WeatherEntry (City cityId' state' cityName' latitude' longitude') date' temperature'
    [] -> do
      maybeWeather <- WeatherApi.getWeather (latitude city) (longitude city)
      currentDay <- liftIO $ utctDay <$> getCurrentTime
      -- TODO handle error better
      (WeatherResponse temperatureResponse) <- either error pure maybeWeather
      liftIO $ putStrLn "Caching weather"
      liftIO $ void $ execute conn "insert into weather (city_id, date, temperature) values (?, CURRENT_DATE, ?)" (cityId, temperatureResponse)
      return $ WeatherEntry city currentDay temperatureResponse
    _ -> error "Multiple weather entries for today" -- TODO log this error but still return

-- Reference for boilerplate: https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
api :: Proxy API
api = Proxy

nt :: Environment -> App a -> Handler a
nt env a = case App.unApp a of
  ReaderT f -> liftIO $ f env

app :: Environment -> Application
app env = simpleCors $ serve api $ hoistServer api (nt env) server

server :: ServerT API App
server = locations :<|> weatherFor

main :: IO ()
main = do
  putStrLn "Starting server..."
  environment <- App.loadEnvironment
  let port = App.port environment
  run port $ app environment
