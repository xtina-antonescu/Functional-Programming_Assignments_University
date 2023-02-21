module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Main exposing (Msg(..))
import Model
import Model.WeatherItems exposing (WeatherItem(..), allSelected, isItemSelected, set)
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import Time exposing (Month(..))
import Util.Time exposing (Date(..))


weatherItemFuzzer : Fuzzer WeatherItem
weatherItemFuzzer =
    Fuzz.oneOfValues [ Temperature, Precipitation, MinMax, CurrentTime ]


suite : Test
suite =
    describe "Main module"
        [ describe "view"
            [ test "view contains 4 checkboxes" <|
                \_ ->
                    Model.initModel Main.devFlags
                        |> (\m ->
                                { m
                                    | state =
                                        Model.HaveWeatherAndTime
                                            { time = Time.millisToPosix 0
                                            , hovering = []
                                            , weather = { utcOffset = 0, hourly = { times = [], temperatures = [], precipitation = [] } }
                                            , selectedItems = allSelected
                                            }
                                }
                           )
                        |> Main.view
                        |> Q.fromHtml
                        |> Q.findAll [ S.class "checkbox", S.containing [ S.tag "input", S.checked True ] ]
                        |> Q.count (Expect.equal 4)
            ]
        , describe "update"
            [ test "The selected weather items state is changed correctly in update" <|
                \item ->
                    Model.initModel Main.devFlags
                        |> (\m ->
                                { m
                                    | state =
                                        Model.HaveWeatherAndTime
                                            { time = Time.millisToPosix 0
                                            , hovering = []
                                            , weather = { utcOffset = 0, hourly = { times = [], temperatures = [], precipitation = [] } }
                                            , selectedItems = allSelected
                                            }
                                }
                           )
                        |> Main.update (ChangeWeatherItemSelection Temperature False)
                        |> (\( m, _ ) ->
                                case m.state of
                                    Model.HaveWeatherAndTime weather ->
                                        weather.selectedItems
                                            |> Expect.all
                                                [ isItemSelected Temperature >> Expect.equal False
                                                , isItemSelected Precipitation >> Expect.equal True
                                                , isItemSelected MinMax >> Expect.equal True
                                                , isItemSelected CurrentTime >> Expect.equal True
                                                ]

                                    _ ->
                                        Expect.fail "the state should still be Model.HaveWeatherAndTime"
                           )
            ]
        , test "getChartData extracts data from the model correctly" <|
            \_ ->
                { time = Time.millisToPosix 0
                , hovering = []
                , weather =
                    { utcOffset = 7200
                    , hourly =
                        { times = [ 1667772000, 1667775600, 1667779200, 1667782800 ]
                        , temperatures = [ 6.9, 6.4, 5.6, 5.4 ]
                        , precipitation = [ 0, 0.1, 0.2, 0.3 ]
                        }
                    }
                , selectedItems = allSelected
                }
                    |> Main.getChartData
                    |> Expect.equal
                        { now = Time.millisToPosix 0
                        , maxTempPoint = Just { time = Time.millisToPosix 1667779200000, temperature = 6.9, precipitation = 0 }
                        , minTempPoint = Just { time = Time.millisToPosix 1667790000000, temperature = 5.4, precipitation = 0.3 }
                        , hourlyPoints =
                            [ { time = Time.millisToPosix 1667779200000, temperature = 6.9, precipitation = 0 }
                            , { time = Time.millisToPosix 1667782800000, temperature = 6.4, precipitation = 0.1 }
                            , { time = Time.millisToPosix 1667786400000, temperature = 5.6, precipitation = 0.2 }
                            , { time = Time.millisToPosix 1667790000000, temperature = 5.4, precipitation = 0.3 }
                            ]
                        , hovering = []
                        , itemsToShow = allSelected
                        }
        , test "getWeeklyData extracts data from the model correctly" <|
            \_ ->
                { time = Time.millisToPosix 0
                , hovering = []
                , weather =
                    { utcOffset = 7200
                    , hourly =
                        { times = List.range 0 48 |> List.map (\i -> i * 3600 + 1667858400)
                        , temperatures =
                            [ 9.9
                            , 9.8
                            , 10.3
                            , 10.3
                            , 10.2
                            , 10.2
                            , 9.9
                            , 9.9
                            , 9.9
                            , 10.1
                            , 10.8
                            , 11.2
                            , 11.5
                            , 11.8
                            , 12.4
                            , 12.6
                            , 12.5
                            , 11.8
                            , 11.1
                            , 10.6
                            , 9.9
                            , 9.3
                            , 9.0
                            , 8.7
                            , 8.2
                            , 7.6
                            , 7.1
                            , 6.8
                            , 6.7
                            , 6.7
                            , 6.4
                            , 6.7
                            , 7.1
                            , 7.7
                            , 9.3
                            , 10.6
                            , 11.2
                            , 11.6
                            , 11.6
                            , 11.5
                            , 11.2
                            , 10.0
                            , 8.7
                            , 7.9
                            , 7.4
                            , 7.1
                            , 6.5
                            , 6.0
                            ]
                        , precipitation =
                            [ 0.1
                            , 0.3
                            , 0.3
                            , 0.5
                            , 0.3
                            , 0.2
                            , 1.3
                            , 0.9
                            , 0.6
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            , 0.0
                            ]
                        }
                    }
                , selectedItems = allSelected
                }
                    |> (Main.getWeeklyData >> .dailyData)
                    |> Expect.equalLists
                        [ { date = Date { day = 8, month = Nov, year = 2022 }, highTemp = Just 12.6, lowTemp = Just 8.7, totalPrecipitaion = 4.5 }
                        , { date = Date { day = 9, month = Nov, year = 2022 }, highTemp = Just 11.6, lowTemp = Just 6, totalPrecipitaion = 0 }
                        ]
        ]
