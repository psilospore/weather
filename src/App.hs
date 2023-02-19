module App where

import Control.Monad.Reader qualified as Reader
import Data.Text
import Database.PostgreSQL.Simple qualified as Simple
import UnliftIO (MonadIO)
import UnliftIO qualified

newtype App a = App {unApp :: Reader.ReaderT Environment IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFail,
      MonadIO,
      Reader.MonadReader Environment,
      UnliftIO.MonadUnliftIO
    )

runApp :: Environment -> App a -> IO a
runApp env app = Reader.runReaderT (unApp app) env

newtype OpenWeatherApiKey = OpenWeatherApiKey {unOpenWeatherApiKey :: Text}

instance Show OpenWeatherApiKey where show _ = "OpenWeatherApiKey <****s*e*c*r*e*t****>"

data Environment = Environment
  { connection :: Simple.Connection,
    openWeatherApiKey :: OpenWeatherApiKey
  }
