module Pages.Workouts.Utils exposing (..)

import Date exposing (Date, Unit(..))
import StrengthSet exposing (StrengthSet)


getSetRanges : List StrengthSet -> ( String, String )
getSetRanges sets =
    let
        repsOnly =
            List.map (\set -> set.reps) sets

        weightsOnly =
            List.map (\set -> set.weight) sets

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


padLists : List a -> List b -> a -> b -> ( List a, List b )
padLists la lb a b =
    let
        paddedLa =
            if List.length la < List.length lb then
                List.repeat (List.length lb - List.length la) a
                    |> List.append la

            else
                la

        paddedLb =
            if List.length la > List.length lb then
                List.repeat (List.length la - List.length lb) b
                    |> List.append lb

            else
                lb
    in
    ( paddedLa, paddedLb )
