module Main exposing (main)

import Api.User exposing (getUser)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Date exposing (Date)
import Html exposing (Html)
import Json.Decode exposing (errorToString)
import Json.Encode as E
import Pages.Login as Login exposing (Msg(..))
import Pages.Workouts.ExercisePageNavigation as Navigation
import Pages.Workouts.WorkoutsPage as Workouts
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser exposing ((</>), oneOf, query, s)
import Url.Parser.Query
import Utils.Error as Error exposing (RequestError(..))
import Utils.Log exposing (LogType(..), log)


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
            Url.Parser.parse (routeParser Navigation.LoadURL) internal
                |> Maybe.map Workouts.Navigate
                |> Maybe.map WorkoutsMessage
                |> Maybe.withDefault (WorkoutsMessage <| Workouts.Navigate <| Navigation.ImproperSelection "Could not properly parse date")

        External _ ->
            WorkoutsMessage <| Workouts.Navigate <| Navigation.ImproperSelection "Unknown link"


onUrlChange : Url -> Msg
onUrlChange url =
    Url.Parser.parse (routeParser (\date -> Navigation.SelectDate date)) url
        |> Maybe.map Workouts.Navigate
        |> Maybe.map WorkoutsMessage
        |> Maybe.withDefault (WorkoutsMessage <| Workouts.Navigate <| Navigation.ImproperSelection "Could not properly parse date")


routeParser : (Date -> Navigation.Action) -> Url.Parser.Parser (Navigation.Action -> a) a
routeParser func =
    oneOf
        [ s "workout"
            </> query (Url.Parser.Query.string "date")
            |> Url.Parser.map (Maybe.map Date.fromIsoString)
            |> Url.Parser.map
                (Maybe.map
                    (\res ->
                        case res of
                            Ok date ->
                                func date

                            Err errMsg ->
                                Navigation.ImproperSelection errMsg
                    )
                )
            |> Url.Parser.map (Maybe.withDefault (Navigation.ImproperSelection "Not cool"))
        ]



-- Model --


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags route key =
    case getUser flags of
        Ok user ->
            let
                thenSelect =
                    Url.Parser.parse (routeParser Navigation.SelectDate) route
            in
            ( WorkoutsPage Workouts.Unauthenticated
            , Task.succeed (Workouts.FetchedUser key thenSelect (Ok user))
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
                    Workouts.update (Workouts.FetchedUser loginMdl.navKey Nothing (Ok user)) Workouts.Unauthenticated
                        |> asMain WorkoutsPage WorkoutsMessage

                _ ->
                    Login.update loginMsg loginMdl |> asMain LoginPage LoginMessage

        ( WorkoutsMessage workoutMsg, WorkoutsPage workoutPage ) ->
            case workoutMsg of
                Workouts.FetchedUser key _ (Err err) ->
                    log Error ("Failed with error " ++ Error.toString err) (LoginPage (Login.empty key))

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
                Workouts.Authenticated data ->
                    { title = "Fit.app " ++ Date.format "EEEE, MMMM d" data.today
                    , body = List.singleton (render model)
                    }

                _ ->
                    { title = "Welcome to Fit.app"
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
