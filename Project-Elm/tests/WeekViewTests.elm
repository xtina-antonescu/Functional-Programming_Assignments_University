module WeekViewTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import Time
import Util.Time exposing (Date(..))
import View.Day as Day
import View.Week as Week


suite : Test
suite =
    let
        sampleDailyData =
            { date = Date { year = 2022, month = Time.Jan, day = 21 }
            , highTemp = Just 0
            , lowTemp = Just 10
            , totalPrecipitaion = 2
            }

        makeWeeklyData : Int -> Week.WeeklyData
        makeWeeklyData nr =
            { dailyData = List.repeat nr sampleDailyData }
    in
    describe "View.Week module"
        [ test "Week view has class week" <|
            \_ ->
                Week.view (makeWeeklyData 7)
                    |> Q.fromHtml
                    |> Q.has [ S.class "week" ]
        , test "Week view has the correct number of days" <|
            \_ ->
                Week.view (makeWeeklyData 7)
                    |> Q.fromHtml
                    |> Q.findAll [ S.class "day" ]
                    |> Q.count (Expect.equal 7)
        , test "Week view contains the start and end dates" <|
            \_ ->
                Week.view
                    { dailyData =
                        [ { date = Date { year = 2020, month = Time.Jan, day = 12 }
                          , highTemp = Just 0
                          , lowTemp = Just 10
                          , totalPrecipitaion = 2
                          }
                        , { date = Date { year = 2022, month = Time.Feb, day = 26 }
                          , highTemp = Just 0
                          , lowTemp = Just 10
                          , totalPrecipitaion = 2
                          }
                        ]
                    }
                    |> Q.fromHtml
                    |> Q.find [ S.tag "h2" ]
                    |> Q.has [ S.text "2022", S.text "2020", S.text "Jan", S.text "Feb", S.text "12", S.text "26" ]
        , test "Week view shows \"No data\" when the list of days is empty" <|
            \_ ->
                Week.view
                    { dailyData = []
                    }
                    |> Q.fromHtml
                    |> Q.find [ S.tag "h2" ]
                    |> Q.has [ S.text "No data" ]
        ]
