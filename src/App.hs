{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Monad.Reader qualified as Reader
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple qualified as Simple
import EnvVars qualified
import LoadEnv (loadEnv)
import System.Envy (decodeEnv)
import UnliftIO (MonadIO)

newtype App a = App {unApp :: Reader.ReaderT Environment IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFail,
      MonadIO,
      Reader.MonadReader Environment
    )

runApp :: Environment -> App a -> IO a
runApp env app = Reader.runReaderT (unApp app) env

newtype OpenWeatherApiKey = OpenWeatherApiKey {unOpenWeatherApiKey :: Text}

instance Show OpenWeatherApiKey where show _ = "OpenWeatherApiKey <****S*e*C*r*E*t*!!!*>"

data Environment = Environment
  { connection :: Simple.Connection,
    openWeatherApiKey :: OpenWeatherApiKey,
    port :: Int
  }

loadEnvironment :: IO Environment
loadEnvironment = do
  loadEnv
  maybeEnvVars <- decodeEnv @EnvVars.EnvVars
  envVars <- case maybeEnvVars of
    Left err -> error $ "Failed to load environment variables: " <> show err
    Right envVars -> pure envVars
  conn <- Simple.connectPostgreSQL $ EnvVars.pgConnString envVars
  pure $ Environment conn (OpenWeatherApiKey $ EnvVars.openWeatherApiKey envVars) (EnvVars.httpPort envVars)
