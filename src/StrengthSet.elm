module StrengthSet exposing (..)

import Date exposing (Date)
import Json.Decode as D
import Json.Encode as E


type alias StrengthSet =
    { reps : Int
    , weight : Float
    }


type alias StrengthExercise =
    { name : String
    , sets : List StrengthSet
    }


type alias LoggedStrenghtSet =
    { todo : StrengthSet
    , logged : Maybe ( Date, StrengthSet )
    }


type alias LoggedStrengthExercise =
    { name : String
    , sets : List LoggedStrenghtSet
    , isSubmitted : Bool
    }


emptySet : StrengthSet
emptySet =
    { reps = 0, weight = 0.0 }


emptyExercise : StrengthExercise
emptyExercise =
    { name = ""
    , sets = []
    }


logSet : Date -> StrengthSet -> LoggedStrenghtSet -> LoggedStrenghtSet
logSet onDate set lset =
    { lset | logged = Just ( onDate, set ) }


markAsLogged : StrengthSet -> LoggedStrenghtSet
markAsLogged set =
    { todo = set, logged = Nothing }


asExercise : LoggedStrengthExercise -> StrengthExercise
asExercise logged =
    { name = logged.name
    , sets = List.map (\loggedSet -> loggedSet.todo) logged.sets
    }



-- asLogged : LoggedStrengthExercise -> ( Date, List StrengthSet )
-- asLogged logged = ( logged.)


logSetInExercise : StrengthSet -> Date -> Int -> LoggedStrengthExercise -> LoggedStrengthExercise
logSetInExercise set onDate setIndex exercise =
    let
        newSets =
            List.indexedMap
                (\index s ->
                    if index == setIndex then
                        logSet onDate set s

                    else
                        s
                )
                exercise.sets
    in
    { exercise | sets = newSets }


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
            List.indexedMap
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
            List.indexedMap
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
        , ( "sets", E.list encodeSet exercise.sets )
        ]


decodeExercise : D.Decoder StrengthExercise
decodeExercise =
    D.map2 StrengthExercise
        (D.field "name" D.string)
        (D.field "sets" (D.list decodeSet))


addLastSet : StrengthExercise -> StrengthExercise
addLastSet exercise =
    List.reverse exercise.sets
        |> List.head
        |> Maybe.map (\set -> { exercise | sets = List.append exercise.sets [ set ] })
        |> Maybe.withDefault exercise


removeSet : Int -> StrengthExercise -> StrengthExercise
removeSet index exercise =
    let
        before =
            List.take index exercise.sets

        after =
            List.drop (index + 1) exercise.sets
    in
    { exercise | sets = List.append before after }
