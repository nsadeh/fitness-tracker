module Routing exposing (Route(..), routePage, routeParser)

import Browser.Navigation as BrowserNav exposing (Key)
import Date exposing (Date)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ((</>))
import Url.Parser.Query as UrlQueryParser


type Route
    = Login
    | PasswordReset String
    | Workout (Maybe Date)


routePage : Key -> Route -> Cmd msg
routePage key route =
    makeUrlString route |> BrowserNav.pushUrl key


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.s "workout"
            </> UrlParser.query (UrlQueryParser.string "date")
            |> UrlParser.map (Maybe.map Date.fromIsoString)
            |> UrlParser.map (Maybe.andThen Result.toMaybe)
            |> UrlParser.map Workout
        , UrlParser.s "login"
            |> UrlParser.map Login
        , UrlParser.fragment (Maybe.withDefault "No token")
            |> (\p ->
                    UrlParser.s "reset_password"
                        </> p
                        |> UrlParser.map (String.dropLeft 13)
                        |> UrlParser.map (String.split "&")
                        |> UrlParser.map List.head
                        |> UrlParser.map (Maybe.withDefault "No token")
                        |> UrlParser.map PasswordReset
               )
        ]


makeUrlString : Route -> String
makeUrlString route =
    case route of
        Login ->
            UrlBuilder.absolute [ "login" ] []

        PasswordReset token ->
            UrlBuilder.absolute [ "reset_password" ] [ UrlBuilder.string "access_token" token ]

        -- /workout?date=2022-01-01
        Workout (Just date) ->
            date
                |> Date.toIsoString
                |> UrlBuilder.string "date"
                |> List.singleton
                |> UrlBuilder.absolute [ "workout" ]

        Workout Nothing ->
            UrlBuilder.absolute [] []
