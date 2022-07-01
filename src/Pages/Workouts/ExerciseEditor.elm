module Pages.Workouts.ExerciseEditor exposing (..)

import Array
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, input, label, span, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import StrengthSet exposing (StrengthExercise, StrengthSet, addLastSet, changeRepCountForExercise, changeWeightForExercise, removeSet)
import Utils.Log exposing (LogType(..), log)
import Utils.OverrideClick exposing (overrideOnClickWith)


type WorkoutEditor
    = Closed
    | Open { id : String, exercise : StrengthExercise, invalidBoxes : Dict Int { reps : Bool, weight : Bool } }

isOpen: WorkoutEditor -> Bool
isOpen editor = case editor of 
    Closed -> False
    Open _ -> True

toggleRepsValidity : Dict Int { reps : Bool, weight : Bool } -> Int -> Bool -> Dict Int { reps : Bool, weight : Bool }
toggleRepsValidity state index isValid =
    case Dict.get index state of
        Nothing ->
            Dict.insert index { reps = isValid, weight = True } state

        Just record ->
            Dict.insert index { record | reps = isValid } state


toggleWeightValidity : Dict Int { reps : Bool, weight : Bool } -> Int -> Bool -> Dict Int { reps : Bool, weight : Bool }
toggleWeightValidity state index isValid =
    case Dict.get index state of
        Nothing ->
            Dict.insert index { reps = True, weight = isValid } state

        Just record ->
            Dict.insert index { record | weight = isValid } state


type EditorMessage
    = Opened String
    | ClosedEditor
    | SetAdded
    | SetRemoved Int
    | ChangedRepCounts Int String
    | ChangedWeight Int String
    | EditSubmitted
    | DeletedSubmitted
    | DoNothing


update :
    { getExercise : String -> Maybe StrengthExercise
    , editExercise : String -> StrengthExercise -> Cmd msg
    , deleteExercise : String -> Cmd msg
    , refresh: String -> Cmd msg
    }
    -> EditorMessage
    -> WorkoutEditor
    -> ( WorkoutEditor, Cmd msg )
update { getExercise, editExercise, deleteExercise, refresh } msg editor =
    case editor of
        Closed ->
            case msg of
                Opened id ->
                    case getExercise id of
                        Just exercise ->  ( Open { id = id, exercise = exercise, invalidBoxes = Dict.empty }, Cmd.none )

                        Nothing -> log Error (id ++ " does not exist in state") editor

                _ ->
                    log Error "Can't close closed editor" editor

        Open exercise ->
            case msg of
                Opened _ ->
                    log Error "Can't open an opened editor" editor

                ClosedEditor ->
                    ( Closed, refresh exercise.id )

                SetAdded ->
                    ( Open { exercise | exercise = addLastSet exercise.exercise }, Cmd.none )

                SetRemoved index ->
                    ( Open { exercise | exercise = removeSet index exercise.exercise }, Cmd.none )

                EditSubmitted ->
                    ( editor, editExercise exercise.id exercise.exercise )

                DeletedSubmitted ->
                    ( editor, deleteExercise exercise.id )

                ChangedRepCounts index maybeReps ->
                    case String.toInt maybeReps of
                        Nothing ->
                            ( Open { exercise | invalidBoxes = toggleRepsValidity exercise.invalidBoxes index False }, Cmd.none )

                        Just reps ->
                            ( Open
                                { exercise
                                    | invalidBoxes = toggleRepsValidity exercise.invalidBoxes index True
                                    , exercise = changeRepCountForExercise index reps exercise.exercise
                                }
                            , Cmd.none
                            )

                ChangedWeight index maybeWeight ->
                    case String.toFloat maybeWeight of
                        Nothing ->
                            ( Open { exercise | invalidBoxes = toggleWeightValidity exercise.invalidBoxes index False }, Cmd.none )

                        Just weight ->
                            ( Open
                                { exercise
                                    | invalidBoxes = toggleWeightValidity exercise.invalidBoxes index True
                                    , exercise = changeWeightForExercise index weight exercise.exercise
                                }
                            , Cmd.none
                            )

                DoNothing ->
                    ( editor, Cmd.none )


view : WorkoutEditor -> Html EditorMessage
view editor =
    case editor of
        Closed ->
            div [] []

        Open { exercise } ->
            div
                [ class "fixed inset-0 flex items-center justify-center bg-gray-800 bg-opacity-50 overflow-y-scroll z-10"
                , onClick ClosedEditor
                ]
                [ div
                    [ class "px-3 border-2 border-blue-400 rounded-md bg-blue-900 h-auto w-auto flex flex-col items-center text-blue-200"
                    , overrideOnClickWith DoNothing
                    ]
                    [ h2 [ class "text-3xl px-5 pt-2 text-blue-200" ]
                        [ text ("Edit " ++ exercise.name)
                        ]
                    , div
                        [ class "flex flex-col justify-center overflow-scroll w-full p-3 mx-auto"
                        ]
                        (Array.indexedMap viewEditorLine exercise.sets |> Array.toList)
                    , div [ class "flex flex-row justify-center w-6/12" ]
                        [ button [ class "border-2 border-blue-400 w-full rounded-md mt-1 mb-3 p-2 hover:bg-blue-400", overrideOnClickWith SetAdded ]
                            [ text "Add set"
                            ]
                        ]
                    , div [ class "flex flex-row justify-between w-6/12" ]
                        [ button [ class "border-2 border-red-400 rounded-md mt-1 mb-3 p-2 hover:bg-red-400 w-5/12", overrideOnClickWith DeletedSubmitted ] [ text "Delete" ]
                        , button [ class "border-2 border-blue-400 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400 w-5/12", overrideOnClickWith EditSubmitted ] [ text "Submit" ]
                        ]
                    ]
                ]


viewEditorLine : Int -> StrengthSet -> Html EditorMessage
viewEditorLine index set =
    div [ class "flex flex-row my-2 h-11" ]
        [ div [ class "pr-2" ]
            [ span []
                [ text (String.fromInt (index + 1) ++ ".") ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ id "reps-editor"
                , class "border rounded-md bg-blue-100 text-black pr-3"
                , type_ "number"
                , value (String.fromInt set.reps)
                , onInput (ChangedRepCounts index)
                ]
                []
            , label [ class "text-md", for ("reps-editor-" ++ String.fromInt index) ] [ text "reps" ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ class "border rounded-md bg-blue-100 text-black pr-3"
                , type_ "number"
                , value (String.fromFloat set.weight)
                , onInput (ChangedWeight index)
                ]
                []
            , label [ class "text-md", for ("weight-editor-" ++ String.fromInt index) ] [ text "lbs" ]
            ]
        , div [ class "flex items-center" ]
            [ button [ class "border-2 border-red-400 w-30 rounded-md hover:bg-red-400", type_ "button", overrideOnClickWith (SetRemoved index) ] [ text "Remove" ]
            ]
        ]
