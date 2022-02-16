module Main exposing (main)

import Browser
import DayOfWeek exposing (DayOfWeek, nextDay, prevDay, stringOfDay)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, h3, h4, input, small, text)
import Html.Attributes exposing (checked, class, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Maybe exposing (withDefault)
import OrderedDict exposing (OrderedDict)
import StrengthSet exposing (StrengthExercise, StrengthSet)
import Workout exposing (Workout, addExercise, expandExercise, liftFormFunction)
import WorkoutCreator exposing (WorkoutCreator, createNew, emptyForm, newSetReps, newSetWeight, toggleCreator, updateName, updateNumSets)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL --


type alias Model =
    { workouts : Dict String Workout
    , current : DayOfWeek
    }


mondayWorkout : OrderedDict String StrengthExercise
mondayWorkout =
    List.map toTuple
        [ { name = "Bench Press", sets = [ { reps = 5, weight = 150 }, { reps = 5, weight = 160 }, { reps = 5, weight = 170 } ], expanded = False }
        , { name = "Back Squat", sets = [ { reps = 10, weight = 195 }, { reps = 10, weight = 195 }, { reps = 10, weight = 195 } ], expanded = False }
        , { name = "Overhead Press", sets = [ { reps = 8, weight = 100 }, { reps = 10, weight = 95 }, { reps = 12, weight = 90 } ], expanded = False }
        ]
        |> OrderedDict.fromList


init : Model
init =
    { workouts =
        Dict.fromList
            [ ( "Monday"
              , { exercises = mondayWorkout
                , creator = emptyForm
                }
              )
            ]
    , current = DayOfWeek.Monday
    }


toTuple : StrengthExercise -> ( String, StrengthExercise )
toTuple ex =
    ( ex.name, ex )


isEditorToggled : Model -> DayOfWeek -> Bool
isEditorToggled model day =
    Dict.get (stringOfDay day) model.workouts
        |> Maybe.map (\workout -> workout.creator.isOpen)
        |> Maybe.withDefault False


getCurrentForm : Model -> WorkoutCreator
getCurrentForm model =
    Dict.get (stringOfDay model.current) model.workouts
        |> Maybe.map (\w -> w.creator)
        |> withDefault emptyForm



-- UPDATE --


type Msg
    = Toggled String
    | Selected DayOfWeek
    | CreateFormToggled
    | SetNumberEntered Int
    | ChangedWorkoutName String
    | UpdatedSetWeight Int Float
    | UpdatedSetReps Int Int
    | ClickedCreatedExercise WorkoutCreator
    | ClearForm


updateWorkout : Model -> String -> (Workout -> Workout) -> Model
updateWorkout model day mapper =
    { model | workouts = Dict.update day (Maybe.map mapper) model.workouts }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggled name ->
            updateWorkout model (stringOfDay model.current) (expandExercise name)

        Selected dayOfWeek ->
            { model | current = dayOfWeek }

        CreateFormToggled ->
            updateWorkout model (stringOfDay model.current) (liftFormFunction toggleCreator)

        SetNumberEntered int ->
            updateWorkout model (stringOfDay model.current) (liftFormFunction (updateNumSets int))

        ChangedWorkoutName name ->
            updateWorkout model (stringOfDay model.current) (liftFormFunction (updateName name))

        UpdatedSetWeight index weight ->
            updateWorkout model (stringOfDay model.current) (liftFormFunction (newSetWeight index weight))

        UpdatedSetReps index reps ->
            updateWorkout model (stringOfDay model.current) (liftFormFunction (newSetReps index reps))

        ClickedCreatedExercise form ->
            updateWorkout model (stringOfDay model.current) (addExercise (createNew form)) 
            |> (\m -> updateWorkout m (stringOfDay model.current) (\w -> { w | creator = emptyForm }))

        ClearForm ->
            updateWorkout model (stringOfDay model.current) (\workout -> { workout | creator = emptyForm })



-- VIEW --


view : Model -> Html Msg
view model =
    let
        exerciseList =
            Dict.get (stringOfDay model.current) model.workouts
                |> Maybe.map (\workout -> workout.exercises)
                |> Maybe.map OrderedDict.values
                |> Maybe.map (List.map viewExercises)
                |> Maybe.withDefault []
    in
    div [ style "padding" "20px" ]
        [ div [ class "row" ]
            [ div [ class "col-lg" ]
                [ div [ class "container-fluid navbar" ]
                    [ button [ class "btn btn-outline-dark", onClick (Selected (prevDay model.current)) ] [ text "<" ]
                    , h2 [ class "mx-auto" ]
                        [ text (stringOfDay model.current) ]
                    , button [ class "btn btn-outline-dark", onClick (Selected (nextDay model.current)) ] [ text ">" ]
                    ]
                , div [] exerciseList
                , input [ type_ "checkbox", class "fake-checkbox", onCheck (\_ -> CreateFormToggled), checked (isEditorToggled model model.current) ] []
                , viewForm (getCurrentForm model)
                , div [ class "d-flex p-2" ]
                    [ button [ class "btn btn-outline-dark mx-auto bg-light", style "width" "90%", onClick CreateFormToggled ]
                        [ if isEditorToggled model model.current then
                            text "-"

                          else
                            text "+"
                        ]
                    ]
                ]
            ]
        ]


viewExercises : StrengthExercise -> Html Msg
viewExercises exercise =
    let
        ( weights, reps ) =
            getSetRanges exercise.sets
    in
    div [ class "container-fluid border border-5 rounded list-group", style "padding-right" "0px", style "margin-bottom" "2px" ]
        [ div
            [ class
                ("list-group-item bg-light border-5"
                    ++ (if exercise.expanded then
                            ""

                        else
                            " rounded-bottom"
                       )
                )
            , onClick (Toggled exercise.name)
            ]
            [ div [ class "row justify-content-between no-gutters", style "white-space" "nowrap" ]
                [ div [ class "container-fluid col-sm-2" ]
                    [ h4 []
                        [ text exercise.name
                        ]
                    ]
                , div [ class "container-fluid col-sm-2" ]
                    [ h4 []
                        [ text (String.fromInt (List.length exercise.sets))
                        , small [ style "font-size" "0.5em" ]
                            [ text "sets"
                            ]
                        ]
                    ]
                , div [ class "col-sm-2" ]
                    [ h4 []
                        [ text weights
                        , small [ style "font-size" "0.5em" ]
                            [ text "lbs"
                            ]
                        ]
                    ]
                , div [ class "col-sm-2" ]
                    [ h4 []
                        [ text reps
                        , small [ style "font-size" "0.5em" ]
                            [ text "reps"
                            ]
                        ]
                    ]
                , div [ class "container-fluid col-sm-2" ]
                    [ button [ type_ "button", class "btn btn-outline-dark float-center" ]
                        [ text "Edit"
                        ]
                    ]
                , div [ class "container-fluid col-sm-2" ]
                    [ button [ type_ "button", class "btn btn-outline-dark float-right" ]
                        [ text "Log all!"
                        ]
                    ]
                ]
            ]
        , input [ type_ "checkbox", class "fake-checkbox", checked exercise.expanded, onCheck (\_ -> Toggled exercise.name) ] []
        , div [ class "slide" ] (List.indexedMap viewSet exercise.sets)
        ]


viewSet : Int -> StrengthSet -> Html Msg
viewSet num set =
    div [ class "container-fluid list-group-item bg-light border-5" ]
        [ div [ class "row", style "white-space" "nowrap" ]
            [ div [ class "container-fluid col-sm-2" ]
                [ h4 [ class "pl-5", style "margin-top" ".5rem" ] [ text (String.fromInt (num + 1) ++ ".") ]
                ]
            , div [ class "col-sm-2" ]
                [ h3 [ style "margin-top" ".5rem" ]
                    [ text (String.fromFloat set.weight)
                    , small [ style "font-size" "0.5em" ] [ text "lbs" ]
                    ]
                ]
            , div [ class "col-sm-2", style "margin-top" ".5rem" ]
                [ input [ type_ "number", class "form-control", value (String.fromFloat set.weight) ] []
                ]
            , div [ class "col-sm-2 container-fluid" ]
                [ h3 [ style "margin-top" ".5rem", style "padding-left" "5rem" ]
                    [ text (String.fromInt set.reps)
                    , small [ style "font-size" "0.5em" ]
                        [ text "reps"
                        ]
                    ]
                ]
            , div [ class "col-sm-2", style "margin-top" ".5rem" ]
                [ input [ type_ "number", class "form-control", value (String.fromInt set.reps) ] []
                ]
            , div [ class "container-fluid col-sm-2", style "margin-top" ".5rem" ]
                [ button [ type_ "button", class "btn btn-outline-dark float-right" ]
                    [ text "Log set"
                    ]
                ]
            ]
        ]


getSetRanges : List StrengthSet -> ( String, String )
getSetRanges sets =
    let
        repsOnly =
            List.map (\set -> set.reps) sets

        weightsOnly =
            List.map (\set -> set.weight) sets

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


viewForm : WorkoutCreator -> Html Msg
viewForm form =
    div [ class "editor" ]
        [ div [ class "list-group border border-5 rounded bg-light" ]
            [ div [ class "list-group-item bg-light" ]
                [ div [ class "container-fluid row", style "margin-bottom" "10px" ]
                    [ div [ class "col-sm-2" ]
                        [ h4 [] [ text "Name: " ]
                        ]
                    , div [ class "col" ]
                        [ input [ class "form-control", placeholder "Name", onInput ChangedWorkoutName ] [] ]
                    ]
                , div [ class "container-fluid row" ]
                    [ div [ class "col-sm-2" ]
                        [ h4 [] [ text "Num sets: " ]
                        ]
                    , div [ class "col" ]
                        [ input [ class "form-control", placeholder "Sets", type_ "number", onInput (\count -> SetNumberEntered (String.toInt count |> withDefault 0)) ] []
                        ]
                    ]
                , div [] [ viewSetForm form ]
                , div [ class "d-flex justify-content-center" ]
                    [ button [ class "btn btn-outline-dark mx-auto", style "margin-top" "30px", style "width" "50%", onClick (ClickedCreatedExercise form) ]
                        [ text "Create set!" ]
                    ]
                ]
            ]
        ]


viewSetForm : WorkoutCreator -> Html Msg
viewSetForm form =
    div [] (List.range 1 form.numSets |> List.map viewFormSingleSet)


viewFormSingleSet : Int -> Html Msg
viewFormSingleSet index =
    div [ class "container-fluid row", style "margin-top" "20px" ]
        [ div [ class "col-sm-2" ]
            [ h4 [] [ text (String.fromInt index ++ ".") ]
            ]
        , div [ class "col-sm-4 flow-row d-flex" ]
            [ h4 [ style "width" "77%", style "margin-right" "10px" ]
                [ text "Starting weight: " ]
            , input [ class "form-control", placeholder "Weight", type_ "number", onInput (\weight -> UpdatedSetWeight index (String.toFloat weight |> withDefault 0)) ] []
            ]
        , div [ class "col-sm-4 d-flex flow-row" ]
            [ h4 [ style "margin-right" "10px" ] [ text "Reps: " ]
            , input [ class "form-control", placeholder "Reps", type_ "number", onInput (\reps -> UpdatedSetReps index (String.toInt reps |> withDefault 0)) ] []
            ]
        ]
