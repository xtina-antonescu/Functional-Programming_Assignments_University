module View.WeatherChart exposing (..)

{-| Module that contains the data and view function definitions for the weather chart.

**Don't modify this module.**

-}

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Chart.Svg as CS
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Model.WeatherData exposing (HourlyDataPoint, toHourlyDataPoints)
import Time
import Util
import Util.Time


type alias Hovering =
    List (CI.One HourlyDataPoint CI.Dot)


type alias MsgMap msg =
    { onHover : List (CI.One HourlyDataPoint CI.Dot) -> msg
    }


type alias ShownItems =
    { temperature : Bool
    , precipitation : Bool
    , minMax : Bool
    , currentTime : Bool
    }


showAllItems : ShownItems
showAllItems =
    { temperature = True
    , precipitation = True
    , minMax = True
    , currentTime = True
    }


type alias ChartData =
    { now : Time.Posix
    , minTempPoint : Maybe HourlyDataPoint
    , maxTempPoint : Maybe HourlyDataPoint
    , hourlyPoints : List HourlyDataPoint
    , hovering : Hovering
    , itemsToShow : ShownItems
    }


view : MsgMap msg -> ChartData -> Html msg
view msgMap { minTempPoint, maxTempPoint, hourlyPoints, hovering, now, itemsToShow } =
    let
        temperatureLabel extraAttrs labelText point =
            point
                |> Maybe.map
                    (\p ->
                        C.label
                            (CA.fontSize 12 :: extraAttrs)
                            [ text <| labelText ++ String.fromFloat p.temperature ++ "°C" ]
                            { x = p.time |> Time.posixToMillis |> toFloat, y = p.temperature }
                    )
                |> Util.maybeToList

        minLabel =
            temperatureLabel [ CA.moveDown 10 ] "Min: " minTempPoint

        maxLabel =
            temperatureLabel [ CA.moveUp 10 ] "Max: " maxTempPoint
    in
    div []
        [ h2 [] [ text "Overview" ]
        , div [ style "width" "30%", style "height" "30%" ]
            [ C.chart
                [ CA.margin { top = 30, bottom = 50, left = 50, right = 30 }
                , CA.height 500
                , CA.width 500
                , CE.onMouseMove msgMap.onHover (CI.dots |> CE.getNearest)
                , CE.onMouseLeave (msgMap.onHover [])
                ]
                (List.concat
                    [ [ C.xTicks [ CA.times Time.utc ]
                      , C.xLabels [ CA.times Time.utc ]
                      , C.yLabels [ CA.withGrid ]
                      , C.yTicks []
                      , C.series (.time >> Time.posixToMillis >> toFloat)
                            (Util.zipFilter
                                [ itemsToShow.temperature, itemsToShow.precipitation ]
                                [ C.interpolated .temperature [ CA.color "orange" ] [] |> C.named "Temperature" |> C.format (\t -> String.fromFloat t ++ "°C")
                                , C.interpolated .precipitation [ CA.color "blue" ] [] |> C.named "Precipitation" |> C.format (\t -> String.fromFloat t ++ "mm")
                                ]
                            )
                            hourlyPoints
                      , C.each hovering <|
                            \p item ->
                                let
                                    color =
                                        CI.getColor item

                                    name =
                                        CI.getName item

                                    value =
                                        CI.getTooltipValue item

                                    data =
                                        CI.getData item
                                in
                                [ C.tooltip item
                                    []
                                    [ style "color" color ]
                                    [ text <| name ++ ": " ++ value, br [] [], text <| Util.Time.formatTime Time.utc data.time ]
                                ]
                      ]
                    , Util.zipFilter [ itemsToShow.currentTime, itemsToShow.minMax, itemsToShow.minMax ] <|
                        List.concat
                            [ [ C.withPlane <|
                                    \p ->
                                        [ C.line
                                            [ CA.x1 (now |> Time.posixToMillis |> toFloat)
                                            , CA.y1 p.y.min
                                            , CA.y2 p.y.max
                                            , CA.dashed [ 5, 5 ]
                                            , CA.color CA.blue
                                            ]
                                        ]
                              ]
                            , minLabel
                            , maxLabel
                            ]
                    ]
                )
            ]
        ]
