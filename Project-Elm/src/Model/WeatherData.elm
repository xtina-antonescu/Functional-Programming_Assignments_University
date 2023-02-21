module Model.WeatherData exposing
    ( ApiHourlyData
    , ApiWeatherData
    , HourlyDataPoint
    , decodeHourlyData
    , decodeWeatherData
    , toHourlyDataPoints
    )

import Json.Decode as De
import Time


{-| Don't modify
-}
type alias HourlyDataPoint =
    { time : Time.Posix, temperature : Float, precipitation : Float }


{-| Don't modify
-}
type alias ApiHourlyData =
    { times : List Int
    , temperatures : List Float
    , precipitation : List Float
    }


{-| Don't modify
-}
type alias ApiWeatherData =
    { hourly : ApiHourlyData
    , utcOffset : Int
    }


{-| Converts the data received from the weather API to a list of hourly datapoints

**Note**: According to the [documentation], the `times` field is received as a **list of UNIX epoch times in seconds relative to GTM+0**.

To handle this:

1.  each timestamp has to be incremented by `utcOffset` (as indicated by the API documentation)
2.  after incrementing, each timestamp must be multipled by 1000 before it is passed to [Time.millisToPosix] (to convert it from seconds to milliseconds)

[documentation]: https://open-meteo.com/en/docs#api-documentation
[Time.Posix]: https://package.elm-lang.org/packages/elm/time/latest/Time#Posix
[Time.millisToPosix]: https://package.elm-lang.org/packages/elm/time/latest/Time#millisToPosix

    import Time

    hourlyData : ApiHourlyData
    hourlyData =
        { times = [ 1667772000, 1667775600, 1667779200, 1667782800 ]
        , temperatures = [ 6.9, 6.4, 5.6, 5.4 ]
        , precipitation = [ 0, 0.1, 0.2, 0.3 ]
        }


    weatherData : ApiWeatherData
    weatherData =
        { hourly = hourlyData
        , utcOffset = 7200
        }

    toHourlyDataPoints weatherData
    --> [ HourlyDataPoint (Time.millisToPosix 1667779200000) 6.9 0
    --> , HourlyDataPoint (Time.millisToPosix 1667782800000) 6.4 0.1
    --> , HourlyDataPoint (Time.millisToPosix 1667786400000) 5.6 0.2
    --> , HourlyDataPoint (Time.millisToPosix 1667790000000) 5.4 0.3
    --> ]

-}

zip3Lists: List a -> List b -> List c -> List (a,b,c)
zip3Lists xl yl zl =
    case (xl, yl, zl) of
        ([], [], []) -> []
        (x::xs, y::ys, z::zs) -> (x,y,z):: zip3Lists xs ys zs
        (_, _, _) -> [] --couldn't compile otherwise, should be fine since the 3 lists should have the same length



toHourlyDataPoints : ApiWeatherData -> List HourlyDataPoint
toHourlyDataPoints receivedData =
    let 
        timesList = List.map (\x -> (x + receivedData.utcOffset) * 1000) receivedData.hourly.times
        temperaturesList = receivedData.hourly.temperatures
        precipitationList = receivedData.hourly.precipitation
    in
        zip3Lists timesList temperaturesList precipitationList 
        |> List.map (\(x, y, z) -> HourlyDataPoint (Time.millisToPosix x) y z)
        |> List.foldr (::) [] 


{-| Decode the hourly data according to <https://open-meteo.com/en/docs#api-documentation>

Relevant fields:

  - time
  - temperature\_2m
  - precipitation

Some relevant functions (see Lab 7 and the [Json.Decode] module documentation for more details):

  - [Json.Decode.list]
  - [Json.Decode.field]
  - [Json.Decode.map3]

[Json.Decode]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
[Json.Decode.list]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#list
[Json.Decode.field]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#field
[Json.Decode.map3]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#map3

-}
decodeHourlyData : De.Decoder ApiHourlyData
decodeHourlyData =
    De.map3 ApiHourlyData 
        (De.field "time" (De.list De.int))
        (De.field "temperature_2m"(De.list De.float))
        (De.field "precipitation" (De.list De.float))


{-| Decode the weather data according to <https://open-meteo.com/en/docs#api-documentation>

Relevant fields:

  - hourly: see `decodeHourlyData`
  - utc\_offset\_seconds

Some relevant functions (see Lab 7 and the [Json.Decode] module documentation for more details):

  - [Json.Decode.field]
  - [Json.Decode.map2]

[Json.Decode]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
[Json.Decode.field]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#field
[Json.Decode.map2]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#map2

-}
decodeWeatherData : De.Decoder ApiWeatherData
decodeWeatherData =
    De.map2 ApiWeatherData 
        (De.field "hourly" decodeHourlyData)
        (De.field "utc_offset_seconds" De.int)
