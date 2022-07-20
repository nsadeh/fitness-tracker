module Pages.Workouts.ExerciseBuilder exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, h4, input, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import StrengthSet exposing (StrengthExercise, StrengthSet)
import Time exposing (Month(..))
import Utils.Log exposing (LogType(..), log)
import Html.Attributes exposing (maxlength)


type alias WorkoutBuilder =
    { isOpen : Bool
    , name : String
    , numSets : Int
    , sets : Dict Int { reps : String, weight : String }
    }



-- FUNCTIONS --


emptyForm : WorkoutBuilder
emptyForm =
    { isOpen = False
    , name = ""
    , numSets = 0
    , sets = Dict.empty
    }


toggleCreator : WorkoutBuilder -> WorkoutBuilder
toggleCreator creator =
    { creator | isOpen = not creator.isOpen }


updateWeight : WorkoutBuilder -> Int -> String -> WorkoutBuilder
updateWeight builder index weight =
    case Dict.get index builder.sets of
        Nothing ->
            { builder | sets = Dict.insert index { reps = "0", weight = weight } builder.sets }

        Just set ->
            { builder | sets = Dict.insert index { reps = set.reps, weight = weight } builder.sets }


updateReps : WorkoutBuilder -> Int -> String -> WorkoutBuilder
updateReps builder index reps =
    case Dict.get index builder.sets of
        Nothing ->
            { builder | sets = Dict.insert index { reps = reps, weight = "0" } builder.sets }

        Just set ->
            { builder | sets = Dict.insert index { reps = reps, weight = set.weight } builder.sets }


isValidSet : { reps : String, weight : String } -> Maybe StrengthSet
isValidSet { reps, weight } =
    Maybe.map2 (\r w -> { reps = r, weight = w }) (String.toInt reps) (String.toFloat weight)


reducer : Maybe StrengthSet -> Maybe (Array StrengthSet) -> Maybe (Array StrengthSet)
reducer set arr =
    Maybe.andThen (\s -> Maybe.map (\a -> Array.push s a) arr) set


parse : Dict Int { reps : String, weight : String } -> Maybe (Array StrengthSet)
parse entries =
    Dict.values entries
        |> List.map isValidSet
        |> List.foldl reducer (Just Array.empty)


submitExercise : WorkoutBuilder -> Msg
submitExercise builder =
    Maybe.map (\sets -> { name = builder.name, sets = sets }) (parse builder.sets)
        |> Maybe.map CreateSubmitted
        |> Maybe.withDefault Invalid



-- Update --


type Msg
    = Opened
    | Closed
    | Cleared
    | SetAdded
    | SetRemoved Int
    | NumSetsEntered Int
    | ChangedName String
    | UpdatedWeight Int String
    | UpdatedReps Int String
    | CreateSubmitted StrengthExercise
    | Invalid


update : { submit : StrengthExercise -> Cmd msg, refresh : Cmd msg } -> Msg -> WorkoutBuilder -> ( WorkoutBuilder, Cmd msg )
update { submit, refresh } msg model =
    case msg of
        Opened ->
            ( { model | isOpen = True }, Cmd.none )

        Closed ->
            ( { model | isOpen = False }, refresh )

        Cleared ->
            ( emptyForm, refresh )

        ChangedName name ->
            ( { model | name = name }, Cmd.none )

        UpdatedWeight index weight ->
            ( updateWeight model index weight, Cmd.none )

        UpdatedReps index reps ->
            ( updateReps model index reps, Cmd.none )

        CreateSubmitted exercise ->
            ( emptyForm, submit exercise )

        SetAdded ->
            ( { model | numSets = model.numSets + 1 }, Cmd.none )

        SetRemoved index ->
            ( { model | numSets = model.numSets - 1, sets = Dict.remove index model.sets }, Cmd.none )

        NumSetsEntered numSets ->
            ( { model | numSets = numSets }, Cmd.none )

        Invalid ->
            log Error "Not implemented Builder.Invalid" model


view : WorkoutBuilder -> Html Msg
view builder =
    div []
        [ div [ class "flex flex-col mx-1 h-fit border border-blue-400 rounded-md max-h-56" ]
            [ div [ class "flex sm:flex-row flex-col sm:justify-around justify-center sm:h-16 h-fit sm:pl-0 w-full" ]
                [ div [ class "flex flex-row justify-center my-auto" ]
                    [ input
                        [ class "border rounded-md bg-blue-100 text-black h-12 w-10/12 px-2 my-4"
                        , placeholder "Name"
                        , onInput ChangedName
                        , value builder.name
                        , maxlength 24
                        ]
                        []
                    ]
                , div [ class "flex flex-row my-auto justify-center" ]
                    [ input
                        [ class "w-10/12 border rounded-md bg-blue-100 text-black h-12 px-2 mb-4"
                        , placeholder "# Sets"
                        , type_ "number"
                        , value
                            (if builder.numSets == 0 then
                                ""

                             else
                                String.fromInt builder.numSets
                            )
                        , onInput (\s -> NumSetsEntered <| Maybe.withDefault 0 <| String.toInt s)
                        ]
                        []
                    ]
                ]
            , div [] [ viewSetForm builder ]
            , div [ class "flex justify-center" ]
                [ button [ class "border-2 border-blue-400 w-30 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400", onClick (submitExercise builder) ]
                    [ text "Create set!" ]
                ]
            ]
        ]


viewSetForm : WorkoutBuilder -> Html Msg
viewSetForm form =
    div [ class "flex flex-col overflow-y-scroll overflow-x-hidden" ] (List.range 1 form.numSets |> List.map viewFormSingleSet)


viewFormSingleSet : Int -> Html Msg
viewFormSingleSet index =
    div [ class "flex flex-row justify-center" ]
        [ div [ class "flex flex-row justify-between pb-3 w-10/12" ]
            [ div [ class "w-6 my-auto" ]
                [ h4 [ class "text-2xl" ] [ text <| String.fromInt index ]
                ]
            , div [ class "flex flow-row" ]
                [ input [ class "sm:w-fit w-32 h-10 flex border rounded-md bg-blue-100 text-black pl-1 pr-3", placeholder "Starting weight", type_ "number", onInput (UpdatedWeight index) ] []
                ]
            , div [ class "flex flow-row" ]
                [ input [ class "sm:w-fit w-32 h-10 flex border rounded-md bg-blue-100 text-black pl-1 pr-3", placeholder "Starting reps", type_ "number", onInput (UpdatedReps index) ] []
                ]
            ]
        ]
