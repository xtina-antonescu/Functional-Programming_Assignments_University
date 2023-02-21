module Model.WeatherItems exposing (SelectedWeatherItems, WeatherItem(..), allSelected, isItemSelected, set, weatherItems)

import Html exposing (..)
import Html.Attributes as HA exposing (class, style)
import Html.Events exposing (..)
import View.WeatherChart exposing (ShownItems)


{-| Items that can be shown or hidden in the weather chart

**Don't modify**

-}
type WeatherItem
    = Temperature
    | Precipitation
    | MinMax
    | CurrentTime


weatherItems : List WeatherItem
weatherItems =
    [ Temperature
    , Precipitation
    , MinMax
    , CurrentTime
    ]


type alias SelectedWeatherItems =
    { temperature : Bool
    , precipitation : Bool
    , minMax : Bool
    , currentTime : Bool
    }


{-| Returns an instance of `SelectedWeatherItems` with all items selected

    isItemSelected Temperature allSelected --> True

    List.all (\i -> isItemSelected i allSelected) weatherItems --> True

-}
allSelected : SelectedWeatherItems
allSelected =
    { temperature = True
    , precipitation = True
    , minMax = True
    , currentTime = True
    }


{-| Given the current state and an `item` it returns whether the `item` is selected.

    isItemSelected Temperature allSelected --> True

-}
isItemSelected : WeatherItem -> SelectedWeatherItems -> Bool
isItemSelected item current =
    case item of
        Temperature ->
            current.temperature

        Precipitation ->
            current.precipitation

        MinMax ->
            current.minMax

        CurrentTime ->
            current.currentTime


{-| Given an `item`, a boolean `value` and the current state, it sets the given `item` in `current` to `value`.

    allSelected |> set Temperature False |> isItemSelected Temperature --> False

    allSelected |> set Precipitation False |> isItemSelected Temperature --> True

-}
set : WeatherItem -> Bool -> SelectedWeatherItems -> SelectedWeatherItems
set item value current =
    case item of
        Temperature ->
            { current | temperature = value }

        Precipitation ->
            { current | precipitation = value }

        MinMax ->
            { current | minMax = value }

        CurrentTime ->
            { current | currentTime = value }
