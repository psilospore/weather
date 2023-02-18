{-# LANGUAGE QuasiQuotes #-}

module Server where

import App (App, Environment (connection))
import Control.Monad.Reader (asks)
import Data.Text
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

newtype State = State Text deriving (Show, Eq, Ord)

newtype Temperature = Temperature Int deriving (Show, Eq, Ord)

data City = City
  { cityName :: Text,
    state :: State,
    latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Eq, Ord)

data WeatherEntry = WeatherEntry
  { city :: City,
    date :: Day,
    temperature :: Temperature
  }
  deriving (Show, Eq)

startServer :: App ()
startServer = undefined

-- List of available cities that have weather data.
-- It can be a hard coded list or cities from a state.
-- From a single state?
locations :: App [City]
locations = do
  conn <- asks connection
  query conn "SELECT (cityName, state, latitude, longitude) FROM cities"

weatherFor :: City -> App WeatherEntry
weatherFor City {cityName} = do
  conn <- asks connection
  weather <-
    query
      conn
      [sql| SELECT cityName, state, latitude, longitude, date, temperature FROM weather
            join cities on weather.city_id = cities.id
            WHERE city = 'hi' and date = CURRENT_DATE |]
      (Only cityName)
  case weather of
    [] -> error "No weather data for today" -- TODO get the current weather, cache it, and return it
    [(cityName, latitude, longitude, date, temperature)] -> pure $ WeatherEntry City {cityName, latitude, longitude} date temperature
    _ -> error "Multiple weather entries for today" -- TODO log this error but still return
