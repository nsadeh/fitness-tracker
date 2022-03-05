module Workout exposing (..)

import StrengthSet exposing (StrengthExercise)
import WorkoutCreator exposing (WorkoutCreator)
import Utils.OrderedDict exposing (OrderedDict)



-- TYPES --


type alias Workout =
    { exercises : OrderedDict String StrengthExercise
    , creator : WorkoutCreator
    }



-- FUNCTIONS --


updateExercise : Workout -> String -> (StrengthExercise -> StrengthExercise) -> Workout
updateExercise workout name mapper =
    { workout | exercises = Utils.OrderedDict.update name (Maybe.map mapper) workout.exercises }


expandExercise : String -> Workout -> Workout
expandExercise name workout =
    updateExercise workout name (\exercise -> { exercise | expanded = not exercise.expanded })


liftFormFunction : (WorkoutCreator -> WorkoutCreator) -> (Workout -> Workout)
liftFormFunction mapper =
    \workout -> { workout | creator = mapper workout.creator }


addExercise : StrengthSet.StrengthExercise -> Workout -> Workout
addExercise exercise workout =
    case Utils.OrderedDict.get exercise.name workout.exercises of
        Just _ ->
            workout

        Nothing ->
            { workout | exercises = Utils.OrderedDict.insert exercise.name exercise workout.exercises }
