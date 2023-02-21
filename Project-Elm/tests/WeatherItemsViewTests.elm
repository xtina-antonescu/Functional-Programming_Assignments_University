module WeatherItemsViewTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Main exposing (Msg(..))
import Model
import Model.WeatherItems exposing (WeatherItem(..), allSelected, isItemSelected, set)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Q
import Test.Html.Selector as S
import Time exposing (Month(..))
import Util.Time exposing (Date(..))
import View.WeatherItems


type Msg
    = OnChange WeatherItem Bool


suite : Test
suite =
    describe "View.WeatherItems module"
        [ test "view contains 4 checkboxes" <|
            \_ ->
                View.WeatherItems.view { onChangeSelection = OnChange } allSelected
                    |> Q.fromHtml
                    |> Q.findAll [ S.class "checkbox", S.containing [ S.tag "input", S.checked True ] ]
                    |> Q.count (Expect.equal 4)
        , test "The correct message is sent when a checkbox is clicked" <|
            \_ ->
                View.WeatherItems.view { onChangeSelection = OnChange } allSelected
                    |> Q.fromHtml
                    |> Q.find [ S.class "checkbox", S.containing [ S.tag "input", S.checked True ], S.containing [ S.text "Temperature" ] ]
                    |> Q.find [ S.tag "input" ]
                    |> Event.simulate (Event.check False)
                    |> Event.expect (OnChange Temperature False)
        ]
