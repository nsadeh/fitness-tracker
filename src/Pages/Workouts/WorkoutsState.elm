module Pages.Workouts.WorkoutsState exposing (..)

import Api.Exercises as Exercises
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict
import Pages.Workouts.ExerciseBuilder exposing (WorkoutBuilder, emptyForm)
import Pages.Workouts.ExerciseEditor exposing (WorkoutEditor(..))
import Pages.Workouts.WorkoutLogger as WorkoutLogger
import Set exposing (Set)
import StrengthSet exposing (LoggedStrengthExercise)
import Swiper
import Time
import Url.Builder
import Utils.OrderedDict exposing (OrderedDict)


type alias WorkoutsPageState =
    { api : Exercises.API
    , user : AuthenticatedUser
    , navKey : Nav.Key
    , today : Date
    , navbarSwipeState : Swiper.SwipingState
    , editor : WorkoutEditor
    , creator : WorkoutBuilder
    , toggled : Set String
    , workout : OrderedDict String LoggedStrengthExercise
    , log : WorkoutLogger.Model
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
    , editor = Closed
    , creator = emptyForm
    , today = Date.fromCalendarDate 2022 Time.Jan 1
    , navKey = navKey
    , navbarSwipeState = Swiper.initialSwipingState
    , log = Dict.empty
    }


refreshUser : WorkoutsPageState -> AuthenticatedUser -> WorkoutsPageState
refreshUser state user =
    { state | user = user, api = Exercises.api url key user }


updateWorkout : WorkoutsPageState -> OrderedDict String LoggedStrengthExercise -> WorkoutsPageState
updateWorkout state workout =
    { state | workout = workout, log = WorkoutLogger.init state.today workout }


updateDate : WorkoutsPageState -> Date -> WorkoutsPageState
updateDate state date =
    { state | today = date, workout = Utils.OrderedDict.empty }


updateBuilder : WorkoutsPageState -> WorkoutBuilder -> WorkoutsPageState
updateBuilder state builder =
    { state | creator = builder }


updateEditor : WorkoutsPageState -> WorkoutEditor -> WorkoutsPageState
updateEditor state editor =
    { state | editor = editor }


updateLog : WorkoutsPageState -> WorkoutLogger.Model -> WorkoutsPageState
updateLog state log =
    { state | log = log }


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
    Nav.pushUrl state.navKey <| formatDateWorkoutURL date


formatDateWorkoutURL : Date -> String
formatDateWorkoutURL date =
    Url.Builder.absolute [ "workout" ] [ Url.Builder.string "date" (Date.toIsoString date) ]
