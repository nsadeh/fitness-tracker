module Pages.Workouts.WorkoutsState exposing (..)

import Api.Exercises as Exercises
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Browser.Navigation as Nav
import Date exposing (Date)
import Pages.Workouts exposing (makeExerciseUrl)
import Set exposing (Set)
import StrengthSet exposing (LoggedStrengthExercise, StrengthExercise)
import Swiper
import Task exposing (Task)
import Time
import Url.Builder
import Utils.OrderedDict exposing (OrderedDict)
import WorkoutCreator exposing (WorkoutCreator, emptyForm)


type alias WorkoutsPageState =
    { api : Exercises.API
    , user : AuthenticatedUser
    , navKey : Nav.Key
    , today : Date
    , navbarSwipeState : Swiper.SwipingState
    , editor : Maybe ( String, StrengthExercise )
    , creator : WorkoutCreator
    , toggled : Set String
    , workout : OrderedDict String LoggedStrengthExercise
    }


type NavbarSwipeDirection
    = Right
    | Left


emptyState : AuthenticatedUser -> Nav.Key -> WorkoutsPageState
emptyState user navKey =
    { api = Exercises.api url key user
    , user = user
    , workout = Utils.OrderedDict.empty
    , toggled = Set.empty
    , editor = Nothing
    , creator = emptyForm
    , today = Date.fromCalendarDate 2022 Time.Jan 1
    , navKey = navKey
    , navbarSwipeState = Swiper.initialSwipingState
    }


refreshUser : WorkoutsPageState -> AuthenticatedUser -> WorkoutsPageState
refreshUser state user =
    { state | user = user, api = Exercises.api url key user }


updateWorkout : WorkoutsPageState -> OrderedDict String LoggedStrengthExercise -> WorkoutsPageState
updateWorkout state workout =
    { state | workout = workout }


updateDate : WorkoutsPageState -> Date -> WorkoutsPageState
updateDate state date =
    { state | today = date, workout = Utils.OrderedDict.empty }


isToggled : WorkoutsPageState -> String -> Bool
isToggled state id =
    Set.member id state.toggled


toggle : WorkoutsPageState -> String -> WorkoutsPageState
toggle state id =
    if isToggled state id then
        { state | toggled = Set.remove id state.toggled }

    else
        { state | toggled = Set.insert id state.toggled }


handleNavbarSwipe : WorkoutsPageState -> Swiper.SwipeEvent -> ( WorkoutsPageState, Maybe NavbarSwipeDirection )
handleNavbarSwipe state event =
    let
        ( _, swipedLeft ) =
            Swiper.hasSwipedLeft event state.navbarSwipeState

        ( nextState, swipedRight ) =
            Swiper.hasSwipedRight event state.navbarSwipeState
    in
    ( { state | navbarSwipeState = nextState }
    , if swipedRight then
        Just Right

      else if swipedLeft then
        Just Left

      else
        Nothing
    )


changeWorkoutURL : WorkoutsPageState -> Date -> Cmd msg
changeWorkoutURL state date =
    Nav.pushUrl state.navKey <| makeExerciseUrl date


formatDateWorkoutURL : Date -> String
formatDateWorkoutURL date =
    Url.Builder.absolute [ "workout" ] [ Url.Builder.string "date" (Date.toIsoString date) ]
