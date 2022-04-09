module WorkoutCreator exposing (..)

import Maybe exposing (withDefault)
import StrengthSet exposing (StrengthSet, emptySet)
import Dict exposing (Dict)


type alias WorkoutCreator =
    { isOpen : Bool
    , name : String
    , numSets : Int
    , sets : Dict Int StrengthSet
    }



-- FUNCTIONS --


emptyForm : WorkoutCreator
emptyForm =
    { isOpen = False
    , name = ""
    , numSets = 0
    , sets = Dict.empty
    }


toggleCreator : WorkoutCreator -> WorkoutCreator
toggleCreator creator =
    { creator | isOpen = not creator.isOpen }


updateNumSets : Int -> WorkoutCreator -> WorkoutCreator
updateNumSets num form =
    { form | numSets = num }


updateName : String -> WorkoutCreator -> WorkoutCreator
updateName name form =
    { form | name = name }


updateSet : Int -> StrengthSet -> WorkoutCreator -> WorkoutCreator
updateSet index set form =
    { form | sets = Dict.insert index set form.sets }


newSetWeight : Int -> Float -> WorkoutCreator -> WorkoutCreator
newSetWeight index weight form =
    let
        newSet =
            Dict.get index form.sets
                |> Maybe.map
                    (\set ->
                        { set | weight = weight }
                    )
                |> withDefault emptySet
    in
    updateSet index newSet form


newSetReps : Int -> Int -> WorkoutCreator -> WorkoutCreator
newSetReps index reps form =
    let
        newSet =
            Dict.get index form.sets
                |> Maybe.map
                    (\set ->
                        { set | reps = reps }
                    )
                |> withDefault emptySet
    in
    updateSet index newSet form


createNew : WorkoutCreator -> StrengthSet.StrengthExercise
createNew form =
    { name = form.name
    , sets = Dict.values form.sets
    }
