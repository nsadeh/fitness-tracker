module Pages.Workouts exposing (..)

import Api.Exercises as Exercise
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, h3, h4, input, small, text)
import Html.Attributes exposing (checked, class, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Maybe exposing (withDefault)
import StrengthSet exposing (StrengthExercise, StrengthSet)
import Time
import Utils.Log exposing (LogType(..), log)
import Utils.OrderedDict
import Workout exposing (Workout, addExercise, expandExercise, liftFormFunction)
import WorkoutCreator exposing (WorkoutCreator, createNew, emptyForm, newSetReps, newSetWeight, toggleCreator, updateName, updateNumSets)
import Date exposing (format)



-- MODEL --


type alias Data =
    { api : Exercise.API
    , currentUser : AuthenticatedUser
    , workouts : Dict String Workout
    , today : Date
    }


type Model
    = Unauthenticated
    | Authenticated Data


isEditorToggled : Data -> Date -> Bool
isEditorToggled model day =
    Dict.get (Date.toIsoString day) model.workouts
        |> Maybe.map (\workout -> workout.creator.isOpen)
        |> Maybe.withDefault False


getCurrentForm : Data -> WorkoutCreator
getCurrentForm model =
    Dict.get (Date.toIsoString model.today) model.workouts
        |> Maybe.map (\w -> w.creator)
        |> withDefault emptyForm



-- UPDATE --


noOp : a -> ( a, Cmd msg )
noOp a =
    ( a, Cmd.none )


type Msg
    = LoggedIn AuthenticatedUser
    | FetchedWorkout Date Workout
    | FetchError Http.Error
    | Toggled String
    | Selected Date
    | CreateFormToggled
    | SetNumberEntered Int
    | ChangedWorkoutName String
    | UpdatedSetWeight Int Float
    | UpdatedSetReps Int Int
    | ClickedCreatedExercise WorkoutCreator
    | ClearForm


updateWorkout : Data -> String -> (Workout -> Workout) -> Model
updateWorkout model day mapper =
    Authenticated { model | workouts = Dict.update day (Maybe.map mapper) model.workouts }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unauthenticated ->
            case msg of
                LoggedIn user ->
                    let
                        authedApi =
                            Exercise.api url key user
                    in
                    ( Authenticated { api = authedApi
                      , currentUser = user
                      , workouts = Dict.empty
                      , today = Date.fromCalendarDate 2022 Time.Mar 7
                      }
                    , authedApi.get ()
                        |> Cmd.map parseWorkout
                    )
                _ -> log Error "Cannot update data on unauthenticated workouts page" model

        Authenticated data ->
            case msg of
                LoggedIn user ->
                    log Info ("user " ++ user.userId ++ " is already logged in") model

                FetchedWorkout today workout ->
                    ( Authenticated { data | workouts = Dict.insert (Date.toIsoString today) workout data.workouts }, Cmd.none )

                FetchError _ ->
                    log Error "error encountered" model

                Toggled name ->
                    updateWorkout data (Date.toIsoString data.today) (expandExercise name) |> log Info ("toggled " ++ name)

                Selected day ->
                    Authenticated { data | today = day } |> noOp

                CreateFormToggled ->
                    updateWorkout data (Date.toIsoString data.today) (liftFormFunction toggleCreator) |> noOp

                SetNumberEntered int ->
                    updateWorkout data (Date.toIsoString data.today) (liftFormFunction (updateNumSets int)) |> noOp

                ChangedWorkoutName name ->
                    updateWorkout data (Date.toIsoString data.today) (liftFormFunction (updateName name)) |> noOp

                UpdatedSetWeight index weight ->
                    updateWorkout data (Date.toIsoString data.today) (liftFormFunction (newSetWeight index weight)) |> noOp

                UpdatedSetReps index reps ->
                    updateWorkout data (Date.toIsoString data.today) (liftFormFunction (newSetReps index reps)) |> noOp

                ClickedCreatedExercise form ->
                    updateWorkout data (Date.toIsoString data.today) (addExercise (createNew form)) |> noOp
                        -- |> (\m -> updateWorkout m.data (Date.toIsoString data.today) (\w -> { w | creator = emptyForm }))
                        -- |> noOp

                ClearForm ->
                    updateWorkout data (Date.toIsoString data.today) (\workout -> { workout | creator = emptyForm }) |> noOp


parseWorkout : Result Http.Error Workout -> Msg
parseWorkout result =
    case result of
        Ok workout ->
            FetchedWorkout (Date.fromCalendarDate 2022 Time.Mar 7) workout

        Err err ->
            FetchError err


prevDay : Date -> Date
prevDay date =
    Date.add Days -1 date


nextDay : Date -> Date
nextDay date =
    Date.add Days 1 date

dateToString: Date -> String
dateToString date = format "EEEE, d MMMM y" date



-- VIEW --


view : Model -> Html Msg
view model =
    case model of
        Unauthenticated ->
            div [] []

        Authenticated workouts ->
            let
                exerciseList =
                    Dict.get (Date.toIsoString workouts.today) workouts.workouts
                        |> Maybe.map (\workout -> workout.exercises)
                        |> Maybe.map Utils.OrderedDict.values
                        |> Maybe.map (List.map viewExercises)
                        |> Maybe.withDefault []
            in
            div [ style "padding" "20px" ]
                [ div [ class "row" ]
                    [ div [ class "col-lg" ]
                        [ div [ class "container-fluid navbar" ]
                            [ button [ class "btn btn-outline-dark", onClick (Selected (prevDay workouts.today)) ] [ text "<" ]
                            , h2 [ class "mx-auto" ]
                                [ text (dateToString workouts.today) ]
                            , button [ class "btn btn-outline-dark", onClick (Selected workouts.today) ] [ text ">" ]
                            ]
                        , div [] exerciseList
                        , input [ type_ "checkbox", class "fake-checkbox", onCheck (\_ -> CreateFormToggled), checked (isEditorToggled workouts workouts.today) ] []
                        , viewForm (getCurrentForm workouts)
                        , div [ class "d-flex p-2" ]
                            [ button [ class "btn btn-outline-dark mx-auto bg-light", style "width" "90%", onClick CreateFormToggled ]
                                [ if isEditorToggled workouts workouts.today then
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
