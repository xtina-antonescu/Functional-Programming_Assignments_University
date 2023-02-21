module DayViewTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import Time
import Util.Time exposing (Date(..), formatDate)
import View.Day as Day


dailyViewSelectors : Day.DailyData -> List S.Selector
dailyViewSelectors dailyData =
    [ S.class "day"
    , S.containing
        [ S.class "day-hightemp"
        , Maybe.map String.fromFloat dailyData.highTemp |> Maybe.withDefault "unavailable" |> S.text
        ]
    , S.containing
        [ S.class "day-lowtemp"
        , Maybe.map String.fromFloat dailyData.lowTemp |> Maybe.withDefault "unavailable" |> S.text
        ]
    , S.containing [ S.class "day-date", S.text (formatDate dailyData.date) ]
    , S.containing [ S.class "day-precipitation", S.text <| String.fromFloat dailyData.totalPrecipitaion ]
    ]


type alias DailyViewFieldTest =
    { description : String
    , data : Day.DailyData
    , testSelectors : Day.DailyData -> List S.Selector
    }


testDayViewMatchesSelectors : DailyViewFieldTest -> Test
testDayViewMatchesSelectors { description, testSelectors, data } =
    test description <|
        \_ ->
            Day.view data
                |> Q.fromHtml
                |> Q.has (S.class "day" :: testSelectors data)


suite : Test
suite =
    let
        sampleDailyData : Day.DailyData
        sampleDailyData =
            { date = Date { year = 2022, month = Time.Jan, day = 21 }
            , highTemp = Just 0
            , lowTemp = Just 10
            , totalPrecipitaion = 2
            }

        sampleDailyDataNoMinMax : Day.DailyData
        sampleDailyDataNoMinMax =
            { date = Date { year = 2022, month = Time.Feb, day = 5 }
            , highTemp = Nothing
            , lowTemp = Nothing
            , totalPrecipitaion = 0
            }
    in
    describe "View.Day module" <|
        [ test "Day view has class day" <|
            \_ ->
                Day.view sampleDailyData
                    |> Q.fromHtml
                    |> Q.has [ S.class "day" ]
        , describe "Day view has required classes" <|
            List.map (\c -> testDayViewMatchesSelectors { description = c, data = sampleDailyData, testSelectors = \_ -> [ S.containing [ S.class c ] ] })
                [ "day-lowtemp"
                , "day-hightemp"
                , "day-date"
                , "day-precipitation"
                ]
        , describe "Day view contains all data fields" <|
            List.map testDayViewMatchesSelectors
                [ { description = "lowTemp"
                  , testSelectors =
                        \d ->
                            [ S.containing
                                [ S.class "day-hightemp"
                                , Maybe.map String.fromFloat d.highTemp |> Maybe.withDefault "unavailable" |> S.text
                                ]
                            ]
                  , data = sampleDailyData
                  }
                , { description = "highTemp"
                  , testSelectors =
                        \d ->
                            [ S.containing
                                [ S.class "day-lowtemp"
                                , Maybe.map String.fromFloat d.lowTemp |> Maybe.withDefault "unavailable" |> S.text
                                ]
                            ]
                  , data = sampleDailyData
                  }
                , { description = "lowTemp (Nothing case)"
                  , testSelectors =
                        \d ->
                            [ S.containing
                                [ S.class "day-hightemp"
                                , Maybe.map String.fromFloat d.highTemp |> Maybe.withDefault "unavailable" |> S.text
                                ]
                            ]
                  , data = sampleDailyDataNoMinMax
                  }
                , { description = "highTemp (Nothing case)"
                  , testSelectors =
                        \d ->
                            [ S.containing
                                [ S.class "day-lowtemp"
                                , Maybe.map String.fromFloat d.lowTemp |> Maybe.withDefault "unavailable" |> S.text
                                ]
                            ]
                  , data = sampleDailyDataNoMinMax
                  }
                , { description = "date"
                  , testSelectors =
                        \d -> [ S.containing [ S.class "day-date", S.text (formatDate d.date) ] ]
                  , data = sampleDailyData
                  }
                , { description = "precipitation"
                  , testSelectors =
                        \d -> [ S.containing [ S.class "day-precipitation", S.text <| String.fromFloat d.totalPrecipitaion ] ]
                  , data = sampleDailyData
                  }
                ]
        ]
