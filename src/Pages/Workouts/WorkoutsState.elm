module Pages.Workouts.WorkoutsState exposing (..)

import Api.Exercises as Exercises
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict
import Pages.Workouts.ExerciseBuilder exposing (WorkoutBuilder, emptyForm)
import Pages.Workouts.ExerciseEditor exposing (WorkoutEditor(..))
import Pages.Workouts.WorkoutLogger as WorkoutLogger exposing (isRelogging)
import Set exposing (Set)
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise)
import Swiper
import Time
import Url.Builder
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)


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
    , workout = OrderedDict.empty
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


updateExercise : WorkoutsPageState -> String -> LoggedStrengthExercise -> WorkoutsPageState
updateExercise state id exercise =
    OrderedDict.update id (Maybe.map (\_ -> exercise)) state.workout
        |> updateWorkout state


updateWorkout : WorkoutsPageState -> OrderedDict String LoggedStrengthExercise -> WorkoutsPageState
updateWorkout state workout =
    { state | workout = workout, log = WorkoutLogger.init state.today workout }


updateDate : WorkoutsPageState -> Date -> WorkoutsPageState
updateDate state date =
    { state | today = date, workout = OrderedDict.empty }


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


isLoggedOn : Date -> WorkoutsPageState -> String -> Bool
isLoggedOn date state id =
    if isRelogging state.log id then
        False

    else
        OrderedDict.get id state.workout
            |> Maybe.map
                (\w ->
                    case w.sets of
                        Unlogged _ ->
                            False

                        Logged { loggedOn } ->
                            date == loggedOn
                )
            |> Maybe.withDefault False
