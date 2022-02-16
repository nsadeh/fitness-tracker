module StrengthSet exposing (..)

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