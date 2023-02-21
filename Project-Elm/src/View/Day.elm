module View.Day exposing (DailyData, view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date)


{-| Don't modify
-}
type alias DailyData =
    { date : Date
    , highTemp : Maybe Float
    , lowTemp : Maybe Float
    , totalPrecipitaion : Float
    }


{-| Generates Html based on `DailyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}

--auxiliary function to display a string from a maybe float
displayMessage: Maybe Float -> String
displayMessage a =
  case a of
    Nothing -> "unavailable"
    Just x -> String.fromFloat x

view : DailyData -> Html msg
view dailyData =
    div [class "day"] 
    [
      h1 [] [
        p [] [text "Daily data:"]
      ],
      div [class "day-date"] [text (Util.Time.formatDate dailyData.date)],
      div [class "day-hightemp"] [text (displayMessage dailyData.highTemp)],
      div [class "day-lowtemp"] [text (displayMessage dailyData.lowTemp)],
      div [class "day-precipitation"] [text (String.fromFloat dailyData.totalPrecipitaion)]
    ]
