module Main exposing (main)

import Api.Supabase exposing (RequestError(..))
import Api.User exposing (getUser)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Date exposing (Date)
import Html exposing (Html)
import Json.Decode exposing (errorToString)
import Json.Encode as E
import Pages.Login as Login exposing (Msg(..))
import Pages.Workouts as Workouts exposing (Model(..), Msg(..), SetupMessage(..))
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser exposing ((</>), oneOf, string)
import Utils.Log exposing (LogType(..), log, logCmd)


main : Program E.Value Model Msg
main =
    Browser.application
        { init = init
        , view = viewDocument view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }



-- Routing --


onUrlRequest : UrlRequest -> Msg
onUrlRequest url =
    case url of
        Internal internal ->
            Url.Parser.parse (routeParser Workouts.LoadedUrl) internal
                |> Maybe.map Select
                |> Maybe.map WorkoutsMessage
                |> Maybe.withDefault (WorkoutsMessage NoOp)

        External _ ->
            WorkoutsMessage NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    Url.Parser.parse (routeParser (\date -> Workouts.Selected date)) url
        |> Maybe.map Select
        |> Maybe.map WorkoutsMessage
        |> Maybe.withDefault (WorkoutsMessage NoOp)


routeParser : (Date -> Workouts.SelectionMessage) -> Url.Parser.Parser (Workouts.SelectionMessage -> a) a
routeParser func =
    oneOf
        [ Url.Parser.s "date"
            </> string
            |> Url.Parser.map Date.fromIsoString
            |> Url.Parser.map
                (\res ->
                    case res of
                        Ok date ->
                            func date

                        Err errMsg ->
                            Workouts.ImporoperSelection errMsg
                )
        ]



-- Model --


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags route key =
    case getUser flags of
        Ok user ->
            let
                thenSelect =
                    Url.Parser.parse (routeParser Workouts.Selected) route
            in
            ( WorkoutsPage Workouts.Unauthenticated
            , Cmd.batch
                [ Task.succeed (Workouts.LoggedIn user key thenSelect)
                    |> Task.map Setup
                    |> Task.perform WorkoutsMessage
                , logCmd Debug "Deserialized user!"
                ]
            )

        Err error ->
            log Info ("Failed with error " ++ errorToString error) (LoginPage (Login.empty key))


type Model
    = LoginPage Login.Model
    | WorkoutsPage Workouts.Model



-- Update --


type Msg
    = LoginMessage Login.Msg
    | WorkoutsMessage Workouts.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoginMessage loginMsg, LoginPage loginMdl ) ->
            case loginMsg of
                LoginSucceeded user ->
                    Workouts.update (LoggedIn user loginMdl.navKey Nothing |> Setup) Unauthenticated |> asMain WorkoutsPage WorkoutsMessage

                _ ->
                    Login.update loginMsg loginMdl |> asMain LoginPage LoginMessage

        ( WorkoutsMessage workoutMsg, WorkoutsPage workoutPage ) ->
            case workoutMsg of
                Setup (FailedRefresh err key) ->
                    log Error ("Failed with error " ++ Workouts.parseError err) (LoginPage (Login.empty key))

                _ ->
                    Workouts.update workoutMsg workoutPage |> asMain WorkoutsPage WorkoutsMessage

        ( _, mdl ) ->
            log Info "Unsupported operation" mdl


asMain : (mdl -> Model) -> (msg -> Msg) -> ( mdl, Cmd msg ) -> ( Model, Cmd Msg )
asMain modelMapper cmdMapper ( mdl, cmd ) =
    ( modelMapper mdl, Cmd.map cmdMapper cmd )



-- View --


viewDocument : (Model -> Html Msg) -> Model -> Document Msg
viewDocument render model =
    case model of
        LoginPage _ ->
            { title = "Welcome to Fit.app"
            , body = List.singleton (render model)
            }

        WorkoutsPage page ->
            case page of
                Unauthenticated ->
                    { title = "Welcome to Fit.app"
                    , body = List.singleton (render model)
                    }

                Authenticated data ->
                    { title = "Fit.app " ++ Date.format "EEEE, MMMM d" data.today
                    , body = List.singleton (render model)
                    }


view : Model -> Html Msg
view model =
    case model of
        LoginPage _ ->
            Login.viewLogin
                |> Html.map LoginMessage

        WorkoutsPage w ->
            Workouts.view w
                |> Html.map WorkoutsMessage
