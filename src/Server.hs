{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import App (App (unApp), Environment (connection), runApp)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text
import Data.Time (Day (..))
import Database.PostgreSQL.Simple (FromRow, Only (..), query, query_)
import Database.PostgreSQL.Simple.FromField (FromField)
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

newtype State = State Text deriving (Show, Eq, Ord, Generic, FromField)

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
  deriving (Show, Eq, Ord, Generic, FromRow)

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
  conn <- asks connection
  liftIO $ query_ conn "SELECT (cityName, state, latitude, longitude) FROM cities"

weatherFor :: Text -> App WeatherEntry
weatherFor cityName = do
  conn <- asks connection
  weather <-
    liftIO $
      query
        conn
        [sql| SELECT cityName, state, latitude, longitude, date, temperature FROM weather
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
nt env a = case unApp a of
  ReaderT f -> liftIO $ f env

app :: Environment -> Application
app env = serve api $ hoistServer api (nt env) server

server :: ServerT API App
server = locations :<|> weatherFor

main :: IO ()
main = do
  let port = 80 -- TODO get this from an environment variable
  mgr <- newManager defaultManagerSettings
  let runApp = run port $ app Environment {connection = undefined, openWeatherApiKey = undefined} -- TODO
  bracket (forkIO runApp) killThread $ \_ -> do
    let getBooksClient :<|> addBookClient = client api
    let printBooks = getBooksClient >>= liftIO . print
    _ <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
      _ <- printBooks
      _ <- addBookClient $ Book "Harry Potter and the Order of the Phoenix"
      _ <- printBooks
      _ <- addBookClient $ Book "To Kill a Mockingbird"
      _ <- printBooks
      _ <- addBookClient $ Book "The Picture of Dorian Gray"
      printBooks
    return ()
