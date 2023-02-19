{-# LANGUAGE OverloadedStrings #-}

module WeatherApi where

import App (App, OpenWeatherApiKey (unOpenWeatherApiKey))
import App qualified as Environment
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.:),
  )
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Witch (into)

newtype WeatherResponse = WeatherResponse Double deriving (Show)

instance FromJSON WeatherResponse where
  parseJSON = withObject "Weather" $ \obj -> do
    main <- obj .: "main"
    temp <- main .: "temp"
    return $ WeatherResponse temp

getWeather :: Double -> Double -> App (Either String WeatherResponse)
getWeather lat lon = do
  apiKey <- asks Environment.openWeatherApiKey
  manager <- liftIO $ newManager tlsManagerSettings
  let url =
        "https://api.openweathermap.org/data/2.5/weather?lat="
          ++ show lat
          ++ "&lon="
          ++ show lon
          ++ "&appid="
          ++ (into $ unOpenWeatherApiKey apiKey)
          ++ "&units=imperial"
  request <- liftIO $ parseRequest url
  response <- liftIO $ httpLbs request manager
  return $ eitherDecode (responseBody response)
