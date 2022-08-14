module StrengthSet exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Json.Decode as D
import Json.Encode as E


type alias StrengthSet =
    { reps : Int
    , weight : Float
    }


type alias StrengthExercise =
    { name : String
    , sets : Array StrengthSet
    }


type LoggableStrengthSet
    = Unlogged StrengthSet
    | Logged StrengthSet Date StrengthSet


type LoggableStrengthExercise
    = LoggableStrengthExercise { name : String, sets : Array LoggableStrengthSet }


make : String -> Array LoggableStrengthSet -> LoggableStrengthExercise
make name_ sets_ =
    LoggableStrengthExercise { name = name_, sets = sets_ }


sets : LoggableStrengthExercise -> Array LoggableStrengthSet
sets (LoggableStrengthExercise args) =
    args.sets


name : LoggableStrengthExercise -> String
name (LoggableStrengthExercise args) =
    args.name


asExercise : LoggableStrengthExercise -> StrengthExercise
asExercise exercise =
    { name = name exercise
    , sets =
        sets exercise
            |> Array.map todo
    }


lastLog : LoggableStrengthSet -> Maybe ( Date, StrengthSet )
lastLog set =
    case set of
        Unlogged _ ->
            Nothing

        Logged _ on done ->
            Just ( on, done )


todo : LoggableStrengthSet -> StrengthSet
todo set =
    case set of
        Unlogged todo_ ->
            todo_

        Logged todo_ _ _ ->
            todo_


emptySet : StrengthSet
emptySet =
    { reps = 0, weight = 0.0 }


emptyExercise : StrengthExercise
emptyExercise =
    { name = ""
    , sets = Array.empty
    }


updateSets : Array LoggableStrengthSet -> LoggableStrengthExercise -> LoggableStrengthExercise
updateSets sets_ (LoggableStrengthExercise args) =
    LoggableStrengthExercise { args | sets = sets_ }


asLoggable : StrengthSet -> LoggableStrengthSet
asLoggable set =
    Unlogged set


changeRepCount : Int -> StrengthSet -> StrengthSet
changeRepCount reps set =
    { set | reps = reps }


changeWeight : Float -> StrengthSet -> StrengthSet
changeWeight weight set =
    { set | weight = weight }


changeRepCountForExercise : Int -> Int -> StrengthExercise -> StrengthExercise
changeRepCountForExercise index reps exercise =
    let
        updatedSets =
            Array.indexedMap
                (\idx value ->
                    if idx == index then
                        changeRepCount reps value

                    else
                        value
                )
                exercise.sets
    in
    { exercise | sets = updatedSets }


changeWeightForExercise : Int -> Float -> StrengthExercise -> StrengthExercise
changeWeightForExercise index weight exercise =
    let
        updatedSet =
            Array.indexedMap
                (\idx set ->
                    if idx == index then
                        changeWeight weight set

                    else
                        set
                )
                exercise.sets
    in
    { exercise | sets = updatedSet }



logSet : Int -> Date -> StrengthSet -> LoggableStrengthExercise -> LoggableStrengthExercise
logSet setNumber on set_ (LoggableStrengthExercise args) =
    args.sets
        |> Array.get setNumber
        |> Maybe.map (markSetLogged on set_)
        |> Maybe.map (\set -> Array.set setNumber set args.sets)
        |> Maybe.withDefault args.sets
        |> (\set -> updateSets set (LoggableStrengthExercise args))


markSetLogged : Date -> StrengthSet -> LoggableStrengthSet -> LoggableStrengthSet
markSetLogged on logged set_ =
    case set_ of
        Unlogged todo_ ->
            Logged todo_ on logged

        Logged todo_ _ _ ->
            Logged todo_ on logged

withName: String -> LoggableStrengthExercise -> LoggableStrengthExercise
withName name_ (LoggableStrengthExercise args) = LoggableStrengthExercise { args | name = name_ }


-- Encoders/Decoders


encodeSet : StrengthSet -> E.Value
encodeSet set =
    E.object
        [ ( "weight", E.float set.weight )
        , ( "reps", E.int set.reps )
        ]


decodeSet : D.Decoder StrengthSet
decodeSet =
    D.map2 StrengthSet
        (D.field "reps" D.int)
        (D.field "weight" D.float)


encodeExercise : StrengthExercise -> E.Value
encodeExercise exercise =
    E.object
        [ ( "name", E.string exercise.name )
        , ( "sets", E.array encodeSet exercise.sets )
        ]


decodeExercise : D.Decoder StrengthExercise
decodeExercise =
    D.map2 StrengthExercise
        (D.field "name" D.string)
        (D.field "sets" (D.array decodeSet))


addLastSet : StrengthExercise -> StrengthExercise
addLastSet exercise =
    Array.toList exercise.sets
        |> List.reverse
        |> List.head
        |> Maybe.map (\set -> { exercise | sets = Array.append exercise.sets (Array.fromList [ set ]) })
        |> Maybe.withDefault exercise


removeSet : Int -> StrengthExercise -> StrengthExercise
removeSet index exercise =
    let
        before =
            List.take index (Array.toList exercise.sets)

        after =
            List.drop (index + 1) (Array.toList exercise.sets)
    in
    { exercise | sets = List.append before after |> Array.fromList }


getSetRanges : List StrengthSet -> ( String, String )
getSetRanges sets_ =
    let
        repsOnly =
            List.map (\set -> set.reps) sets_

        weightsOnly =
            List.map (\set -> set.weight) sets_

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
