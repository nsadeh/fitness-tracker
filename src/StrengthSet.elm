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


type alias LoggedStrenghtSet =
    { todo : StrengthSet
    , logged : Maybe ( Date, StrengthSet )
    }


type alias LoggedStrengthExercise =
    { name : String
    , sets : LoggableStrengthSets
    , loggedOn : Maybe Date
    }


type LoggableStrengthSets
    = Unlogged { todo : Array StrengthSet }
    | Logged { loggedOn : Date, sets : Array { todo : StrengthSet, logged : StrengthSet } }


emptySet : StrengthSet
emptySet =
    { reps = 0, weight = 0.0 }


emptyExercise : StrengthExercise
emptyExercise =
    { name = ""
    , sets = Array.empty
    }


logSetsInExercise : Date -> List StrengthSet -> LoggedStrengthExercise -> LoggedStrengthExercise
logSetsInExercise date sets exercise =
    let
        updatedSets =
            case exercise.sets of
                Unlogged { todo } ->
                    Logged { loggedOn = date, sets = (List.map2 (\t s -> { todo = t, logged = s }) (Array.toList todo) sets) |> Array.fromList }

                Logged logged ->
                    logSetsInExercise date sets { name = exercise.name, sets = Unlogged { todo = Array.map (\t -> t.todo) logged.sets }, loggedOn = exercise.loggedOn }
                        |> (\ex -> ex.sets)
    in
    { exercise | loggedOn = Just date, sets = updatedSets }


logSet : Date -> StrengthSet -> LoggedStrenghtSet -> LoggedStrenghtSet
logSet onDate set lset =
    { lset | logged = Just ( onDate, set ) }


markAsLogged : StrengthSet -> LoggedStrenghtSet
markAsLogged set =
    { todo = set, logged = Nothing }


asExercise : LoggedStrengthExercise -> StrengthExercise
asExercise logged =
    { name = logged.name
    , sets =
        case logged.sets of
            Unlogged { todo } ->
                todo

            Logged { sets } ->
                Array.map (\set -> set.todo) sets
    }


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


numSets : LoggableStrengthSets -> Int
numSets s =
    case s of
        Unlogged { todo } ->
            Array.length todo

        Logged { sets } ->
            Array.length sets
