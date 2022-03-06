module Workout exposing (..)

import Utils.OrderedDict exposing (OrderedDict)
import Utils.OrderedDict as OrderedDict
import StrengthSet exposing (StrengthExercise)

type alias ExerciseName = String
type alias Workout = OrderedDict ExerciseName StrengthExercise

addExercise : Workout -> StrengthSet.StrengthExercise -> Workout
addExercise workout exercise =
    case OrderedDict.get exercise.name workout of
        Just _ ->
            workout

        Nothing ->
            OrderedDict.insert exercise.name exercise workout

expandExercise: String -> Workout -> Workout
expandExercise name workout = 
    updateExercise name (\exercise -> { exercise | expanded = not exercise.expanded }) workout


updateExercise : String -> (StrengthExercise -> StrengthExercise) -> Workout -> Workout
updateExercise name mapper workout = OrderedDict.update name (Maybe.map mapper) workout
