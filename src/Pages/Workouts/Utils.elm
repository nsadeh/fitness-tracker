module Pages.Workouts.Utils exposing (..)

import StrengthSet exposing (LoggedStrenghtSet)
import Date exposing (Date)
import Date exposing (Unit(..))


getSetRanges : List LoggedStrenghtSet -> ( String, String )
getSetRanges sets =
    let
        repsOnly =
            List.map (\set -> set.todo.reps) sets

        weightsOnly =
            List.map (\set -> set.todo.weight) sets

        minReps =
            List.minimum repsOnly |> Maybe.withDefault 0

        maxReps =
            List.maximum repsOnly |> Maybe.withDefault 0

        minWeights =
            List.minimum weightsOnly |> Maybe.withDefault 0

        maxWeights =
            List.maximum weightsOnly |> Maybe.withDefault 0

        weightString =
            if minWeights == maxWeights then
                String.fromFloat minWeights

            else
                String.fromFloat minWeights ++ "-" ++ String.fromFloat maxWeights

        repString =
            if minReps == maxReps then
                String.fromInt minReps

            else
                String.fromInt minReps ++ "-" ++ String.fromInt maxReps
    in
    ( weightString, repString )


prevDay : Date -> Date
prevDay date =
    Date.add Days -1 date


nextDay : Date -> Date
nextDay date =
    Date.add Days 1 date


dateToString : Date -> String
dateToString date =
    Date.format "EEE, MMMM d" date
