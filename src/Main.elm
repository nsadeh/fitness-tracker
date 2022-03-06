module Main exposing (main)

import Api.User exposing (getUser)
import Browser exposing (Document)
import Html exposing (Html)
import Json.Decode exposing (errorToString)
import Json.Encode as E
import Pages.Login as Login exposing (Msg(..))
import Pages.Workouts as Workouts exposing (Model(..), Msg(..))
import Task
import Utils.Log exposing (LogType(..), log)


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , view = viewDocument view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model --


init : E.Value -> ( Model, Cmd Msg )
init flags =
    case getUser flags of
        Ok user ->
            ( WorkoutsPage Workouts.Unauthenticated, Task.succeed (Workouts.LoggedIn user) |> Task.perform WorkoutsMessage )

        Err error ->
            log Info ("Failed with error " ++ errorToString error) (LoginPage Login.empty)


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
                    Workouts.update (LoggedIn user) Unauthenticated |> asMain WorkoutsPage WorkoutsMessage

                _ ->
                    Login.update loginMsg loginMdl |> asMain LoginPage LoginMessage

        ( WorkoutsMessage workoutMsg, WorkoutsPage workoutPage ) ->
            case workoutMsg of
                FailedRefresh err ->
                    log Error ("Failed with error " ++ Workouts.parseError err) (LoginPage Login.empty)

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
