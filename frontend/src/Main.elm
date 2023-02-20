module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (..)
import Time 


-- MODEL

type alias City = 
  { cityId : Int,
    cityName : String,
    state : String,
    latitude : Float,
    longitude : Float 
  }

type alias Model =
    { cities: List City
    , selectedCity : Maybe Int
    , weather : Maybe WeatherEntry
    , error : Maybe Http.Error
    , loading : Bool
    }


type alias WeatherEntry = 
  { city : City,
    date : String,
    temperature : Float
  }



initialModel : Model
initialModel =
    { weather = Nothing
    , selectedCity = Nothing
    , error = Nothing
    , cities = []
    , loading = True
    }


-- UPDATE


type Msg
    =
    CitySelected (Maybe Int)
    | LoadCities (Result Http.Error (List City))
    | SetWeather (Result Http.Error WeatherEntry)
    | ClearError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadCities (Ok cities) -> 
            ( {model | cities = cities, loading = False}, Cmd.none )
        LoadCities (Err err) -> 
            ( {model | cities = [], loading = False, error = Just err}, Cmd.none )
        CitySelected city ->
            ( { model | weather = Nothing, error = Nothing, selectedCity = city },
                case city of
                    Just id -> fetchWeather id 
                    Nothing -> Cmd.none
            )
        SetWeather (Ok weather) ->
            ( { model | weather = Just weather, error = Nothing }, Cmd.none )
        SetWeather (Err err) ->
            ( { model | weather = Nothing, error = Just err }, Cmd.none )

        ClearError ->
            ( { model | error = Nothing }, Cmd.none )

fetchCities : Cmd Msg
fetchCities =
    let
        url =
            "localhost:80/locations/"
    in
    Http.get
        { url = url
        , expect = Http.expectJson SetWeather weatherEntryDecoder
        }

fetchWeather : Int -> Cmd Msg
fetchWeather cityId =
    let
        url =
            "localhost:80/location/" ++ String.fromInt cityId ++ "/1/weather"
    in
    Http.get
        { url = url
        , expect = Http.expectJson SetWeather weatherEntryDecoder
        }

andMap = Json.Decode.map2 (|>)

cityDecoder : Decoder City
cityDecoder =
    succeed City
        |> andMap (field "cityId" int)
        |> andMap (field "cityName" string)
        |> andMap (field "state" string)
        |> andMap (field "latitude" float)
        |> andMap (field "longitude" float)

weatherEntryDecoder : Decoder WeatherEntry
weatherEntryDecoder =
    succeed WeatherEntry
        |> andMap (field "city" cityDecoder)
        |> andMap (field "date" string)
        |> andMap (field "temperature" float)

-- VIEW



view : Model -> Html Msg
view model =
    div [] (
        if model.loading then
            [p [ ] [ text "LOADING..." ]]
        else 
            [ h1 [] [ text "Weather App" ]
            , case model.error of
                Just err -> p [] [text <| httpErrorToString err]
                Nothing -> div [] []
            , div [] [ select [ onInput (\cityId -> CitySelected (String.toInt cityId)) ]
                (option [] [ text "Select a city" ]
                    :: (List.map cityOption model.cities)
                )
            ]
            , case model.weather of
                Just weather ->
                    div []
                        [ p [] [ text <| "Temperature: " ++ String.fromFloat weather.temperature ++ "°F" ]
                        ]
                Nothing -> div [] []
            ])

cityOption : City -> Html Msg
cityOption city =
    let
        optionValue =
            String.fromInt city.cityId

        optionText =
            city.cityName ++ ", " ++ city.state
    in
    option [ Html.Attributes.value (optionValue)] [ text optionText ]

-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, fetchCities )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- TODO extract msg
httpErrorToString : Http.Error -> String
httpErrorToString err = "Http Error"