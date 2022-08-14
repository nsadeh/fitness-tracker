module Main exposing (main)

import Api.Exercises as Exercise
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Api.User as Users exposing (getUser)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Date
import Effects exposing (Effect(..))
import Html exposing (div)
import Http as H
import Json.Encode as E
import Pages.Login as Login exposing (Msg(..))
import Pages.PasswordReset as PasswordReset exposing (Msg(..))
import Pages.Workouts.ExercisePageNavigation as Navigation
import Pages.Workouts.WorkoutsPage as Workouts
import Routing as Routes exposing (Route(..), routeParser)
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser
import Utils.Error as Error exposing (RequestError(..))
import Utils.Log exposing (LogLevel(..), logCmd)


main : Program E.Value Model Msg
main =
    Browser.application
        { init = init
        , view = viewDocument
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }



-- Routing --
-- someone clicks a link --


doRoute : Route -> Msg
doRoute route =
    case route of
        Routes.Login ->
            LoginMsg Login.LoadLogin

        PasswordReset token ->
            PasswordResetMsg (PasswordReset.SetToken token)

        Workout (Just date) ->
            date
                |> Navigation.SelectDate
                |> Workouts.Navigate
                |> WorkoutsMsg

        Workout Nothing ->
            Navigation.SelectToday
                |> Workouts.Navigate
                |> WorkoutsMsg


onUrlRequest : UrlRequest -> Msg
onUrlRequest url =
    case url of
        Internal internal ->
            onUrlChange internal

        External _ ->
            WorkoutsMsg <| Workouts.Navigate (Navigation.ImproperSelection "Unknown link")



-- the url changes (as in pushurl) --


onUrlChange : Url -> Msg
onUrlChange url =
    url
        |> Url.Parser.parse routeParser
        |> Maybe.map doRoute
        |> Maybe.withDefault (LoginMsg Login.LoadLogin)



-- Model --


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case getUser flags of
        Ok user ->
            case Url.Parser.parse Routes.routeParser url of
                Just Routes.Login ->
                    ( login key, Cmd.none )

                Just (Routes.PasswordReset token) ->
                    ( unauthedPage key (ResetPasswordPage (PasswordReset.fromToken token)), Cmd.none )

                Just (Routes.Workout (Just date)) ->
                    ( authedPage key user (WorkoutsPage (Workouts.loading date)), Cmd.none )

                Just (Routes.Workout Nothing) ->
                    ( authedPage key user Loading, Task.perform loadWorkouts Date.today )

                Nothing ->
                    ( authedPage key user Loading, Task.perform loadWorkouts Date.today )

        Err _ ->
            case Url.Parser.parse Routes.routeParser url of
                Just (Routes.PasswordReset token) ->
                    ( unauthedPage key (ResetPasswordPage (PasswordReset.fromToken token)), Cmd.none )

                _ ->
                    ( login key, Cmd.none )


type AuthenticatedPage
    = WorkoutsPage Workouts.Model
    | Loading


type UnauthenticatedPage
    = Login Login.Model
    | ResetPasswordPage PasswordReset.Model


type Page
    = Unauthd UnauthenticatedPage
    | Authed AuthenticatedUser AuthenticatedPage


type Model
    = Model Key Page


login : Key -> Model
login key =
    Model key (Unauthd (Login Login.empty))


authedPage : Key -> AuthenticatedUser -> AuthenticatedPage -> Model
authedPage key user page_ =
    Model key (Authed user page_)


unauthedPage : Key -> UnauthenticatedPage -> Model
unauthedPage key page =
    Model key (Unauthd page)



-- Update --


type Msg
    = LoginMsg Login.Msg
    | WorkoutsMsg Workouts.Msg
    | PasswordResetMsg PasswordReset.Msg
    | RefreshUser Effect
    | RefreshedUser Effect AuthenticatedUser
    | FailedFetch Error.RequestError


loadWorkouts : Date.Date -> Msg
loadWorkouts date =
    date
        |> Navigation.SelectDate
        |> Workouts.Navigate
        |> WorkoutsMsg


refreshAuthIfNeeded : Effect -> Error.RequestError -> Msg
refreshAuthIfNeeded thenDo error =
    case error of
        Http (H.BadStatus 401) ->
            RefreshUser thenDo

        _ ->
            FailedFetch error


postErrorHandler : Date.Date -> Effect -> Result Error.RequestError () -> Msg
postErrorHandler date effect result =
    Error.handle (\_ -> loadWorkouts date) (\err -> refreshAuthIfNeeded effect err) result


runEffect : Model -> Effect -> Cmd Msg
runEffect (Model key page) effect =
    let
        _ =
            Users.api url Api.Supabase.key
    in
    case page of
        Authed user _ ->
            let
                workouts =
                    Exercise.api url Api.Supabase.key user
            in
            case effect of
                FetchWorkout date ->
                    date
                        |> workouts.getLoggedWorkouts
                        |> Task.attempt (Error.handle (\w -> Workouts.FetchedWorkout w |> WorkoutsMsg) (refreshAuthIfNeeded effect))

                FetchExercise date exerciseId ->
                    exerciseId
                        |> workouts.getExercise date
                        |> Task.attempt (Error.handle (\ex -> Workouts.FetchedExercise exerciseId ex |> WorkoutsMsg) (refreshAuthIfNeeded effect))

                DeleteExercise date exerciseId ->
                    workouts.deleteExercise date exerciseId
                        |> Task.attempt (postErrorHandler date effect)

                CreateExercise date exercise order ->
                    workouts.insert date { exercise = exercise, order = order, day = Date.weekday date }
                        |> Task.attempt (postErrorHandler date effect)

                EditExerciseName date exerciseId name ->
                    workouts.editName date exerciseId name
                        |> Task.attempt (postErrorHandler date effect)

                EditExercise date exerciseId sets ->
                    workouts.editSets date exerciseId sets
                        |> Task.attempt (postErrorHandler date effect)

                ChangeExerciseOrder _ _ _ ->
                    Cmd.none

                LogWorkout date exerciseId sets ->
                    workouts.logExercise date exerciseId sets
                        |> Task.attempt (postErrorHandler date effect)

                Logout ->
                    Cmd.none

                Log level message ->
                    logCmd level message

                RouteTo route ->
                    Routes.routePage key route

                LoadTodayWorkout ->
                    Date.today
                        |> Task.andThen workouts.getLoggedWorkouts
                        |> Task.attempt (Error.handle (\w -> Workouts.FetchedWorkout w |> WorkoutsMsg) (refreshAuthIfNeeded effect))

        Unauthd _ ->
            Debug.todo "branch 'Unauthd _' not implemented"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model key page) =
    case ( msg, page ) of
        ( RefreshedUser thenDo newUser, Authed user p ) ->
            ( Model key (Authed newUser p), Cmd.batch [ Users.storeUser user, runEffect (Model key page) thenDo ] )

        ( RefreshUser thenDo, Authed user _ ) ->
            let
                api =
                    Users.api url Api.Supabase.key

                refreshed =
                    api.refreshAuth user.refreshToken
                        |> Task.attempt (Error.handle (RefreshedUser thenDo) FailedFetch)
            in
            ( Model key page, refreshed )

        ( FailedFetch err, _ ) ->
            Utils.Log.log Error ("Encountered error: \n" ++ Error.toString err) (Model key page)

        ( LoginMsg Login.LoadLogin, _ ) ->
            ( login key, Cmd.none )

        ( LoginMsg (Login.LoginSucceeded user), _ ) ->
            Task.perform loadWorkouts Date.today
                |> Tuple.pair (authedPage key user Loading)

        ( LoginMsg loginMsg, Unauthd (Login loginPage) ) ->
            Login.update loginMsg loginPage
                |> Tuple.mapBoth (\p -> unauthedPage key (Login p)) (\m -> Cmd.map LoginMsg m)

        ( WorkoutsMsg (Workouts.Navigate (Navigation.SelectDate date)), Authed user _ ) ->
            runEffect (Model key page) (FetchWorkout date)
                |> Tuple.pair (authedPage key user (WorkoutsPage (Workouts.loading date)))

        ( WorkoutsMsg workoutsMsg, Authed user (WorkoutsPage workoutsPage) ) ->
            let
                ( newPage, effects, _ ) =
                    Workouts.update workoutsMsg workoutsPage
            in
            ( authedPage key user (WorkoutsPage newPage), Cmd.batch (List.map (runEffect (Model key page)) effects) )

        ( PasswordResetMsg (PasswordReset.SetToken token), _ ) ->
            ( unauthedPage key (ResetPasswordPage (PasswordReset.fromToken token)), Cmd.none )

        ( PasswordResetMsg passwordResetMsg, Unauthd (ResetPasswordPage resetPasswordPage) ) ->
            PasswordReset.update passwordResetMsg resetPasswordPage
                |> Tuple.mapBoth (\p -> unauthedPage key (ResetPasswordPage p)) (\cmd -> Cmd.map PasswordResetMsg cmd)

        ( _, _ ) ->
            ( Model key page, Cmd.none )



-- View --


viewDocument : Model -> Document Msg
viewDocument (Model _ page) =
    case page of
        Unauthd (Login model) ->
            { title = "Welcome to OnTrack"
            , body = [ Login.view model |> Html.map LoginMsg ]
            }

        Unauthd (ResetPasswordPage model) ->
            { title = "OnTrack"
            , body = [ PasswordReset.view model |> Html.map PasswordResetMsg ]
            }

        Authed _ (WorkoutsPage model) ->
            { title = "OnTrack " ++ Date.format "EEE, MMM d" (Workouts.dateOf model)
            , body = [ Workouts.view model |> Html.map WorkoutsMsg ]
            }

        Authed _ Loading ->
            { title = "OnTrack", body = [ div [] [] ] }
