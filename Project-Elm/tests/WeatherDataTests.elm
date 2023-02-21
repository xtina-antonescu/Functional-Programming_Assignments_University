module WeatherDataTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as De
import Model.WeatherData as WD
import Test exposing (..)


hourlyData : WD.ApiHourlyData
hourlyData =
    { times = [ 1667772000, 1667775600, 1667779200, 1667782800 ]
    , temperatures = [ 6.9, 6.4, 5.6, 5.4 ]
    , precipitation = [ 0, 0, 0, 0 ]
    }


weatherData : WD.ApiWeatherData
weatherData =
    { hourly = hourlyData
    , utcOffset = 7200
    }


suite : Test
suite =
    let
        hourlyDataJsonStr =
            """{
                  "time": [1667772000, 1667775600, 1667779200, 1667782800],
                  "temperature_2m": [6.9, 6.4, 5.6, 5.4],
                  "precipitation": [0, 0, 0, 0]
                }"""

        weatherDataJsonStr =
            """{
                 "latitude": 46.75,
                 "longitude": 23.625,
                 "utc_offset_seconds": 7200,
                 "hourly": """
                ++ hourlyDataJsonStr
                ++ "}"
    in
    describe "Model.WeatherData module"
        [ test "decodeHourlyData works correctly" <|
            \_ ->
                De.decodeString WD.decodeHourlyData hourlyDataJsonStr |> Expect.equal (Ok hourlyData)
        , test "decodeWeatherData works correctly" <|
            \_ ->
                De.decodeString WD.decodeWeatherData weatherDataJsonStr |> Expect.equal (Ok weatherData)
        , test "decodeHourlyData returns an error for invalid data" <|
            \_ ->
                De.decodeString WD.decodeHourlyData "invalid" |> Expect.err
        , test "decodeWeatherData returns an error for invalid data" <|
            \_ ->
                De.decodeString WD.decodeWeatherData "invalid" |> Expect.err
        ]
