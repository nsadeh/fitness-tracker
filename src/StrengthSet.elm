module StrengthSet exposing (..)

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


emptySet : StrengthSet
emptySet =
    { reps = 0, weight = 0.0 }


emptyExercise : StrengthExercise
emptyExercise =
    { name = ""
    , sets = []
    }


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
        before = List.take index exercise.sets
        after = List.drop (index + 1) exercise.sets
    in
        { exercise | sets = List.append before after }