module StrengthSet exposing (..)
import Json.Encode as E
import Json.Decode as D
import Array

type alias StrengthSet =
    { reps : Int
    , weight : Float
    }


type alias StrengthExercise =
    { name : String
    , sets : List StrengthSet
    , expanded : Bool
    }

emptySet : StrengthSet 
emptySet = { reps = 0, weight = 0.0 }

encodeSet: StrengthSet -> E.Value
encodeSet set = E.object
    [ ("weight", E.float set.weight)
    , ("reps", E.int set.reps)
    ]

decodeSet: D.Decoder StrengthSet
decodeSet = D.map2 StrengthSet
    ( D.field "reps" D.int )
    ( D.field "weight" D.float )


encodeExercise: StrengthExercise -> E.Value
encodeExercise exercise = E.object
    [ ("name", E.string exercise.name)
    , ("sets", E.list encodeSet exercise.sets)
    ]

decodeExercise : D.Decoder StrengthExercise
decodeExercise = (D.map3 StrengthExercise
    ( D.field "name" D.string)
    ( D.field "sets" (D.list decodeSet)))
    ( D.succeed False )

