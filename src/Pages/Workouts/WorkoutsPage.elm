module Pages.Workouts.WorkoutsPage exposing (..)

import Api.Supabase exposing (AuthenticatedUser)
import Api.User exposing (storeUser)
import Browser.Navigation as Nav
import Date exposing (Date)
import Pages.Workouts.Utils exposing (nextDay, prevDay)
import Pages.Workouts.WorkoutsState exposing (NavbarSwipeDirection(..), WorkoutsPageState, changeWorkoutURL, emptyState, handleNavbarSwipe, refreshUser, toggle, updateDate, updateWorkout)
import StrengthSet exposing (LoggedStrengthExercise)
import Swiper
import Task
import Utils.Error as Error exposing (RequestError)
import Utils.Log exposing (LogType(..), log, logCmd)
import Utils.OrderedDict exposing (OrderedDict)


type Model
    = Unauthenticated
    | Authenticated WorkoutsPageState


type Msg
    = PageSetup SetupMessage
    | LoadData PageSelectionMessage



-- Setup page


type SetupMessage
    = LoggedIn AuthenticatedUser Nav.Key (Maybe PageSelectionMessage)
    | FetchedWorkout (OrderedDict String LoggedStrengthExercise)
    | FetchError RequestError
    | FailedRefresh RequestError Nav.Key


handleSetup : SetupMessage -> Model -> ( Model, Cmd Msg )
handleSetup msg model =
    case model of
        Unauthenticated ->
            case msg of
                LoggedIn user navKey thenLoad ->
                    let
                        model =
                            Authenticated (emptyState user navKey)

                        ( loaded, next ) =
                            loadWorkoutsData thenLoad model
                    in
                    ( loaded, Cmd.batch [ next, storeUser user ] )

                _ ->
                    log Error "Must be logged in to setup workouts page" model

        Authenticated state ->
            case msg of
                LoggedIn user _ _ ->
                    ( refreshUser state user |> Authenticated, Cmd.batch [ logCmd Info "refreshed user", storeUser user ] )

                FetchedWorkout workout ->
                    ( Authenticated (updateWorkout state workout), Cmd.none )

                FetchError error ->
                    log Error ("Fetch Error: " ++ Error.toString error) model

                FailedRefresh err _ ->
                    log Error ("Refresh Error: " ++ Error.toString err) model


loadWorkoutsData : Maybe PageSelectionMessage -> Model -> ( Model, Cmd Msg )
loadWorkoutsData loadMsg model =
    case loadMsg of
        Just msg ->
            handleSelection msg model

        Nothing ->
            ( model, Task.perform (\date -> LoadedUrl date |> LoadData) Date.today )



-- Select elements on page --


type PageSelectionMessage
    = Toggled String
    | Selected Date
    | LoadedUrl Date
    | Swiped Swiper.SwipeEvent
    | ImporoperSelection String


handleSelection : PageSelectionMessage -> Model -> ( Model, Cmd Msg )
handleSelection msg model =
    case model of
        Unauthenticated ->
            log Error "Unauthenticated user" model

        Authenticated state ->
            case msg of
                Toggled exerciseID ->
                    ( Authenticated (toggle state exerciseID), Cmd.none )

                Selected date ->
                    ( Authenticated <| updateDate state date, Task.attempt parseWorkout <| state.api.getLoggedWorkouts date )

                LoadedUrl date ->
                    ( model, changeWorkoutURL state date )

                Swiped event ->
                    let
                        ( swipedState, swipeDirection ) =
                            handleNavbarSwipe state event
                    in
                    case swipeDirection of
                        Nothing ->
                            ( Authenticated swipedState, Cmd.none )

                        Just direction ->
                            handleSelection
                                (case direction of
                                    Left ->
                                        LoadedUrl <| prevDay swipedState.today

                                    Right ->
                                        LoadedUrl <| nextDay swipedState.today
                                )
                                (Authenticated swipedState)

                ImporoperSelection err ->
                    log Error err model

parseWorkout : Result RequestError (OrderedDict String LoggedStrengthExercise) -> Msg
parseWorkout result =
    case result of
        Ok workout ->
            FetchedWorkout workout |> PageSetup

        Err err ->
            FetchError err |> PageSetup
