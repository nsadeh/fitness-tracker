module Pages.Workouts.ExercisePageNavigation exposing (NavAction(..), navigatePage, expand)

import Actions as Actions exposing (Action(..))
import Date exposing (Date)
import Effects as Effects exposing (Effect(..), withEffect)
import Pages.Workouts.Utils exposing (nextDay, prevDay)
import Pages.Workouts.WorkoutsState as State exposing (State)
import Swiper
import Utils.Log exposing (LogLevel(..))


type NavbarSwipeDirection
    = Left
    | Right

expand: String -> NavAction
expand exerciseId = ExpandExercise exerciseId

-- Update --

type NavAction
    = ExpandExercise String
    | SelectDate Date
    | SelectToday
    | SwipedNavbar Swiper.SwipeEvent
    | ImproperSelection String


navigatePage : NavAction -> State -> ( State, List Effect, List Action )
navigatePage action state =
    case action of
        ExpandExercise id ->
            state
                |> State.toggle id
                |> Effects.none
                |> Actions.none

        SelectDate date ->
            state
                |> State.updateDate date
                |> withEffect (FetchWorkout date)
                |> Actions.none

        SwipedNavbar event ->
            let
                ( swipedState, swipeDirection ) =
                    handleNavbarSwipe state event
            in
            case swipeDirection of
                Nothing ->
                    swipedState
                        |> Effects.none
                        |> Actions.none

                Just direction ->
                    case direction of
                        Left ->
                            swipedState
                                |> Effects.none
                                |> Actions.addAction (GoToWorkout <| prevDay <| State.today swipedState)

                        Right ->
                            swipedState
                                |> Effects.none
                                |> Actions.addAction (GoToWorkout <| nextDay <| State.today swipedState)

        ImproperSelection selection ->
            state
                |> Effects.withEffect (Log Error selection)
                |> Actions.none

        SelectToday ->
            state
                |> Effects.withEffect LoadTodayWorkout
                |> Actions.none
            


handleNavbarSwipe : State -> Swiper.SwipeEvent -> ( State, Maybe NavbarSwipeDirection )
handleNavbarSwipe state event =
    let
        ( _, swipedLeft ) =
            State.swipeState state
                |> Swiper.hasSwipedLeft event

        ( nextState, swipedRight ) =
            State.swipeState state
                |> Swiper.hasSwipedRight event
    in
    ( State.updateSwipe nextState state
    , if swipedRight then
        Just Right

      else if swipedLeft then
        Just Left

      else
        Nothing
    )
