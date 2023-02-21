module Model exposing
    ( AppState(..)
    , Config
    , Mode(..)
    , Model
    , Weather
    , initModel
    )

{- Data definitions the app state

   **Don't modify this module.**

-}

import Model.WeatherData exposing (ApiWeatherData, HourlyDataPoint)
import Model.WeatherItems exposing (SelectedWeatherItems)
import Time
import View.WeatherChart exposing (Hovering)


type alias Weather =
    { time : Time.Posix
    , weather : ApiWeatherData
    , hovering : Hovering
    , selectedItems : SelectedWeatherItems
    }


type AppState
    = WaitingForTime
    | HaveTime { time : Time.Posix }
    | HaveWeatherAndTime Weather
    | FailedToLoad


type alias Model =
    { config : Config
    , state : AppState
    }


type Mode
    = Dev
    | Prod


type alias Config =
    { apiUrl : String
    , mode : Mode
    }


initModel : Config -> Model
initModel config =
    { config = config, state = WaitingForTime }
