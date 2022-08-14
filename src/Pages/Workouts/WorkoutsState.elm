module Pages.Workouts.WorkoutsState exposing
    ( State
    , builder
    , deleteExercise
    , editor
    , isToggled
    , logger
    , new
    , swipeState
    , today
    , toggle
    , updateBuilder
    , updateDate
    , updateEditor
    , updateExercise
    , updateLogger
    , updateSwipe
    , updateWorkout
    , workout
    )

import Date exposing (Date)
import Pages.Workouts.ExerciseBuilder as Builder
import Pages.Workouts.ExerciseEditor as Editor
import Pages.Workouts.WorkoutLogger as Logger
import Set exposing (Set)
import StrengthSet exposing (LoggableStrengthSet(..), LoggableStrengthExercise)
import Swiper exposing (SwipingState)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)


type alias StateArgs =
    { today : Date
    , workout : OrderedDict String LoggableStrengthExercise
    , editor : Editor.Model
    , builder : Builder.Model
    , logger : Logger.Model
    , expanded : Set String
    , swipe : SwipingState
    }


type State
    = State StateArgs



-- Update state --


new : Date -> OrderedDict String LoggableStrengthExercise -> State
new today_ workout_ =
    State
        { today = today_
        , workout = workout_
        , editor = Editor.closed
        , builder = Builder.closed
        , logger = Logger.new today_ workout_
        , expanded = Set.empty
        , swipe = Swiper.initialSwipingState
        }


updateWorkout : OrderedDict String LoggableStrengthExercise -> State -> State
updateWorkout workout_ (State args) =
    State { args | workout = workout_ }


updateExercise : String -> LoggableStrengthExercise -> State -> State
updateExercise exerciseId exercise (State args) =
    State { args | workout = OrderedDict.insert exerciseId exercise args.workout }


deleteExercise : String -> State -> State
deleteExercise exerciseId (State args) =
    State { args | workout = OrderedDict.remove exerciseId args.workout }


updateDate : Date -> State -> State
updateDate date (State args) =
    State { args | today = date }


updateEditor : State -> Editor.Model -> State
updateEditor (State args) editor_ =
    State { args | editor = editor_ }


updateBuilder : State -> Builder.Model -> State
updateBuilder (State args) builder_ =
    State { args | builder = builder_ }


updateLogger : State -> Logger.Model -> State
updateLogger (State args) logger_ =
    State { args | logger = logger_ }


updateSwipe : Swiper.SwipingState -> State -> State
updateSwipe swipe (State args) =
    State { args | swipe = swipe }


toggle : String -> State -> State
toggle exerciseId (State args) =
    if Set.member exerciseId args.expanded then
        State { args | expanded = Set.remove exerciseId args.expanded }

    else
        State { args | expanded = Set.insert exerciseId args.expanded }



-- Get state --


today : State -> Date
today (State args) =
    args.today


workout : State -> OrderedDict String LoggableStrengthExercise
workout (State args) =
    args.workout


editor : State -> Editor.Model
editor (State args) =
    args.editor


builder : State -> Builder.Model
builder (State args) =
    args.builder


logger : State -> Logger.Model
logger (State args) =
    args.logger


isToggled : String -> State -> Bool
isToggled exerciseId (State args) =
    Set.member exerciseId args.expanded


swipeState : State -> Swiper.SwipingState
swipeState (State args) =
    args.swipe
