module Pages.Workouts.ExerciseEditor exposing (Model, Msg, closed, isOpen, open, update, view)

import Array
import Date exposing (Date)
import Dict exposing (Dict)
import Effects as Effects exposing (Effect(..), addEffect)
import Html exposing (Html, button, div, h2, input, label, span, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import StrengthSet exposing (StrengthExercise, StrengthSet, addLastSet, changeRepCountForExercise, changeWeightForExercise, removeSet)
import Utils.Log exposing (LogLevel(..))
import Utils.OverrideClick exposing (overrideOnClickWith)


type alias BoxValidators =
    Dict Int { reps : Bool, weight : Bool }


type alias EditorArgs =
    { exerciseId : String, exercise : StrengthExercise, invalidBoxes : BoxValidators, date : Date }


type Model
    = Closed
    | Open EditorArgs


closed : Model
closed =
    Closed


open : String -> StrengthExercise -> Date -> Msg
open exerciseId exercise date =
    Opened date exerciseId exercise


updateExercise : EditorArgs -> StrengthExercise -> Model
updateExercise args exercise =
    Open { args | exercise = exercise }


updateValidators : BoxValidators -> Model -> Model
updateValidators validators editor =
    case editor of
        Closed ->
            Closed

        Open args ->
            Open { args | invalidBoxes = validators }


updateRepNumber : EditorArgs -> Int -> Maybe Int -> Model
updateRepNumber args setNumber reps =
    case reps of
        Just repNumber ->
            changeRepCountForExercise setNumber repNumber args.exercise
                |> updateExercise args
                |> updateValidators (toggleRepsValidity args.invalidBoxes setNumber False)

        Nothing ->
            toggleRepsValidity args.invalidBoxes setNumber True
                |> (\val -> updateValidators val (Open args))


updateWeightNumber : EditorArgs -> Int -> Maybe Float -> Model
updateWeightNumber args setNumber weight =
    case weight of
        Just weightNumber ->
            changeWeightForExercise setNumber weightNumber args.exercise
                |> updateExercise args
                |> updateValidators (toggleWeightValidity args.invalidBoxes setNumber False)

        Nothing ->
            toggleWeightValidity args.invalidBoxes setNumber True |> (\val -> updateValidators val (Open args))


isOpen : Model -> Bool
isOpen editor =
    case editor of
        Closed ->
            False

        Open _ ->
            True


toggleRepsValidity : BoxValidators -> Int -> Bool -> BoxValidators
toggleRepsValidity state index isValid =
    case Dict.get index state of
        Nothing ->
            Dict.insert index { reps = isValid, weight = True } state

        Just record ->
            Dict.insert index { record | reps = isValid } state


toggleWeightValidity : BoxValidators -> Int -> Bool -> BoxValidators
toggleWeightValidity state index isValid =
    case Dict.get index state of
        Nothing ->
            Dict.insert index { reps = True, weight = isValid } state

        Just record ->
            Dict.insert index { record | weight = isValid } state


type Msg
    = Opened Date String StrengthExercise
    | ClosedEditor
    | SetAdded
    | SetRemoved Int
    | ChangedRepCounts Int String
    | ChangedWeight Int String
    | EditSetsSubmitted
    | DeletedSubmitted
    | DoNothing


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case model of
        Closed ->
            case msg of
                Opened date id exercise ->
                    ( Open { exerciseId = id, exercise = exercise, invalidBoxes = Dict.empty, date = date }, [] )

                _ ->
                    ( model, [ Log Info "Editor is already closed" ] )

        Open editor ->
            case msg of
                Opened date newID newExercise ->
                    update (Opened date newID newExercise) Closed

                ClosedEditor ->
                    Effects.none Closed

                SetAdded ->
                    addLastSet editor.exercise
                        |> updateExercise editor
                        |> Effects.none

                SetRemoved setNumber ->
                    removeSet setNumber editor.exercise
                        |> updateExercise editor
                        |> Effects.none

                ChangedRepCounts setNumber repString ->
                    String.toInt repString
                        |> updateRepNumber editor setNumber
                        |> Effects.none

                ChangedWeight setNumber weightString ->
                    String.toFloat weightString
                        |> updateWeightNumber editor setNumber
                        |> Effects.none

                EditSetsSubmitted ->
                    ( model, [] )
                        |> addEffect (EditExercise editor.date editor.exerciseId <| Array.toList editor.exercise.sets)
                        |> addEffect (FetchExercise editor.date editor.exerciseId)

                DeletedSubmitted ->
                    ( model, [] )
                        |> addEffect (DeleteExercise editor.date editor.exerciseId)
                        |> addEffect (FetchExercise editor.date editor.exerciseId)

                DoNothing ->
                    model |> Effects.none


view : Model -> Html Msg
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
                    [ h2 [ class "sm:text-3xl text-xl px-5 pt-2 text-blue-200" ]
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
                        , button [ class "border-2 border-blue-400 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400 w-5/12", overrideOnClickWith EditSetsSubmitted ] [ text "Submit" ]
                        ]
                    ]
                ]


viewEditorLine : Int -> StrengthSet -> Html Msg
viewEditorLine index set =
    div [ class "flex flex-row my-2 h-11" ]
        [ div [ class "pr-2" ]
            [ span []
                [ text (String.fromInt (index + 1) ++ ".") ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ id "reps-editor"
                , class "border rounded-md bg-blue-100 text-black pr-3 w-16"
                , type_ "number"
                , value (String.fromInt set.reps)
                , onInput (ChangedRepCounts index)
                ]
                []
            , label [ class "text-md", for ("reps-editor-" ++ String.fromInt index) ] [ text "reps" ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ class "border rounded-md bg-blue-100 text-black pr-3 w-16"
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
