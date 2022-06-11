module Pages.Workouts.WorkoutsState exposing (WorkoutsPageState)

import Api.Exercises as Exercises
import Api.Supabase exposing (AuthenticatedUser)
import Date exposing (Date)
import Browser.Navigation as Nav
import Swiper
import StrengthSet exposing (StrengthExercise)
import WorkoutCreator exposing (WorkoutCreator)
import Set exposing (Set)
import Utils.OrderedDict exposing (OrderedDict)
import StrengthSet exposing (LoggedStrengthExercise)

type alias WorkoutsPageState =
    { api : Exercises.API
    , user : AuthenticatedUser
    , navKey : Nav.Key
    , today : Date
    , swipeState : Swiper.SwipingState
    , editor : Maybe ( String, StrengthExercise )
    , creator : WorkoutCreator
    , open : Set String
    , workout : OrderedDict String LoggedStrengthExercise
    }

isToggled: WorkoutsPageState -> String -> Bool
isToggled state id = Set.member id state.open