{-# LANGUAGE OverloadedStrings #-}

module WeatherApi where

import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.:),
  )
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

newtype WeatherResponse = WeatherResponse Double deriving (Show)

instance FromJSON WeatherResponse where
  parseJSON = withObject "Weather" $ \obj -> do
    main <- obj .: "main"
    temp <- main .: "temp"
    return $ WeatherResponse temp

getWeather :: Double -> Double -> String -> IO (Either String WeatherResponse)
getWeather lat lon apiKey = do
  manager <- newManager tlsManagerSettings
  let url =
        "https://api.openweathermap.org/data/2.5/weather?lat="
          ++ show lat
          ++ "&lon="
          ++ show lon
          ++ "&appid="
          ++ apiKey
          ++ "&units=imperial"
  request <- parseRequest url
  response <- httpLbs request manager
  return $ eitherDecode (responseBody response)
