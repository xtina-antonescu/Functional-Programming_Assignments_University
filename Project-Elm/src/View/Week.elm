module View.Week exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date, formatDate)
import View.Day exposing (DailyData)


type alias WeeklyData =
    { dailyData : List DailyData
    }


{-| Generates Html based on `WeeklyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}

renderList: List DailyData -> List (Html msg)
renderList ls = 
  case ls of
    [] -> [text "No data"]
    _ -> List.map View.Day.view ls

view : WeeklyData -> Html msg
view weeklyData =
    div [class "week"]
        [ h2 [] 
          (renderList weeklyData.dailyData)
        ]
    
