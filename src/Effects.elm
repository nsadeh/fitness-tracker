module Effects exposing (Effect(..), addEffect, none, withEffect)

import Date exposing (Date)
import Routing exposing (Route)
import StrengthSet exposing (StrengthExercise, StrengthSet)
import Utils.Log exposing (LogLevel)


type Effect
    = FetchWorkout Date
    | FetchExercise Date String
    | DeleteExercise Date String
    | CreateExercise Date StrengthExercise Int
    | EditExerciseName Date String String
    | EditExercise Date String (List StrengthSet)
    | ChangeExerciseOrder Date String Int
    | LogWorkout Date String (List StrengthSet)
    | LogSet Date String Int StrengthSet
    | Logout
    | Log LogLevel String
    | RouteTo Route
    | LoadTodayWorkout


none : a -> ( a, List Effect )
none a =
    ( a, [] )


addEffect : Effect -> ( a, List Effect ) -> ( a, List Effect )
addEffect effect output =
    Tuple.mapSecond (\effects -> effect :: effects) output


withEffect : Effect -> a -> ( a, List Effect )
withEffect effect a =
    ( a, [ effect ] )
