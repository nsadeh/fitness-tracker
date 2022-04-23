module Main exposing (main)

import Api.User exposing (getUser)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Date
import Html exposing (Html)
import Json.Decode exposing (errorToString)
import Json.Encode as E
import Pages.Login as Login exposing (Msg(..))
import Pages.Workouts as Workouts exposing (Model(..), Msg(..), SetupMessage(..))
import Task
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils.Log exposing (LogType(..), log)
import Api.Supabase exposing (RequestError(..))
import Url.Parser exposing (Parser)
import Json.Decode exposing (oneOf)
import Url.Parser exposing (s)


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
            WorkoutsMessage <| CreateNew <| Workouts.ChangedWorkoutName internal.path

        External external ->
            WorkoutsMessage <| CreateNew <| Workouts.ChangedWorkoutName external


onUrlChange : Url -> Msg
onUrlChange url =
    let
        dateResult =
            String.dropLeft 1 url.path
                |> Date.fromIsoString
    in
    case dateResult of
        Ok date ->
            WorkoutsMessage <| Select <| Workouts.LoadedUrl date

        Err errorMsg ->
            WorkoutsMessage <| Setup <| Workouts.FetchError (NavError errorMsg)


-- routeParser: Parser (Route -> a) -> a
-- routeParser = oneOf [
--     map Workouts.Selected (s "date" </> Date.Date )
-- ]


-- Model --


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags _ key =
    case getUser flags of
        Ok user ->
            ( WorkoutsPage Workouts.Unauthenticated
            , Task.succeed (Workouts.LoggedIn user key)
                |> Task.map Setup
                |> Task.perform WorkoutsMessage
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
                    Workouts.update (LoggedIn user loginMdl.navKey |> Setup) Unauthenticated |> asMain WorkoutsPage WorkoutsMessage

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
    { title = "Fit.app"
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
