module Pages.Workouts exposing (..)

import Api.Exercises as Exercise exposing (InsertExerciseRequest, api, generateExerciseID)
import Api.Supabase exposing (AuthenticatedUser, key, url)
import Api.User exposing (refresh, storeUser)
import Date exposing (Date, Unit(..), format, weekday)
import Html exposing (Html, button, div, h2, h3, h4, input, small, text)
import Html.Attributes exposing (checked, class, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http as H
import Maybe exposing (withDefault)
import StrengthSet exposing (StrengthExercise, StrengthSet)
import Time
import Utils.Log exposing (LogType(..), log, logCmd)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)
import Workout exposing (Workout, expandExercise)
import WorkoutCreator exposing (WorkoutCreator, createNew, emptyForm, newSetReps, newSetWeight, toggleCreator, updateName, updateNumSets)



-- MODEL --


type alias Data =
    { api : Exercise.API
    , currentUser : AuthenticatedUser
    , workout : OrderedDict String StrengthExercise
    , form : WorkoutCreator
    , today : Date
    }


type Model
    = Unauthenticated
    | Authenticated Data


isEditorToggled : Data -> Bool
isEditorToggled data =
    data.form.isOpen



-- UPDATE --


noOp : a -> ( a, Cmd msg )
noOp a =
    ( a, Cmd.none )


type Msg
    = LoggedIn AuthenticatedUser
    | FetchedWorkout Workout
    | FetchError H.Error
    | FailedRefresh H.Error
    | Toggled String
    | Selected Date
    | CreateFormToggled
    | SetNumberEntered Int
    | ChangedWorkoutName String
    | UpdatedSetWeight Int Float
    | UpdatedSetReps Int Int
    | ClickedCreateExercise
    | CreateNewExercise String
    | InsertError String
    | LogSet String StrengthSet
    | LoggedSet String Int
    | DeleteExercise String
    | ClearForm


updateWorkout : (Workout -> Workout) -> Data -> Data
updateWorkout mapper data =
    { data | workout = mapper data.workout }


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
                    ( Authenticated
                        { api = authedApi
                        , currentUser = user
                        , workout = OrderedDict.empty
                        , form = emptyForm
                        , today = Date.fromCalendarDate 2022 Time.Mar 7
                        }
                    , Cmd.batch [ storeUser user, Cmd.map parseWorkout (authedApi.get (Date.fromCalendarDate 2022 Time.Mar 7)) ]
                    )

                _ ->
                    log Error "Cannot update data on unauthenticated workouts page" model

        Authenticated data ->
            case msg of
                LoggedIn user ->
                    ( Authenticated { data | currentUser = user }, Cmd.batch [ logCmd Info "refreshed user", storeUser user ] )

                FailedRefresh err ->
                    log Error ("error encountered" ++ parseError err) model

                FetchedWorkout workout ->
                    ( Authenticated { data | workout = workout }, Cmd.none )

                FetchError err ->
                    case err of
                        H.BadStatus 401 ->
                            ( model, refresh url key data.currentUser.refreshToken |> Cmd.map parseLogin )

                        _ ->
                            log Error ("error encountered" ++ parseError err) model

                Toggled name ->
                    Authenticated (updateWorkout (expandExercise name) data) |> log Info ("toggled " ++ name)

                Selected day ->
                    ( Authenticated { data | today = day, workout = OrderedDict.empty, form = emptyForm }
                    , let
                        authedApi =
                            api url key data.currentUser
                      in
                      Cmd.map parseWorkout (authedApi.get day)
                    )

                CreateFormToggled ->
                    Authenticated { data | form = toggleCreator data.form } |> noOp

                SetNumberEntered int ->
                    Authenticated { data | form = updateNumSets int data.form } |> noOp

                ChangedWorkoutName name ->
                    Authenticated { data | form = updateName name data.form } |> noOp

                UpdatedSetWeight index weight ->
                    Authenticated { data | form = newSetWeight index weight data.form } |> noOp

                UpdatedSetReps index reps ->
                    Authenticated { data | form = newSetReps index reps data.form } |> noOp

                ClickedCreateExercise ->
                    ( model, Cmd.map CreateNewExercise <| generateExerciseID )

                CreateNewExercise uuid ->
                    ( model, data.api.insert (newExerciseBody uuid data) |> Cmd.map parseInsertResults )

                InsertError err ->
                    log Error ("Error while inserting exercise log " ++ err) model

                LogSet id set ->
                    ( model, data.api.logSet id set |> Cmd.map parseInsertResults )

                LoggedSet _ _ ->
                    log Info "In the future we will grey out logged sets" model

                DeleteExercise id ->
                    ( model, data.api.deleteExercise id (Date.fromCalendarDate 2022 Time.Mar 7) |> Cmd.map parseInsertResults )

                ClearForm ->
                    ( Authenticated { data | form = emptyForm }
                    , let
                        authedApi =
                            api url key data.currentUser
                      in
                      Cmd.map parseWorkout (authedApi.get data.today)
                    )


parseWorkout : Result H.Error Workout -> Msg
parseWorkout result =
    case result of
        Ok workout ->
            FetchedWorkout workout

        Err err ->
            FetchError err


parseLogin : Result H.Error AuthenticatedUser -> Msg
parseLogin result =
    case result of
        Ok user ->
            LoggedIn user

        Err err ->
            FailedRefresh err


parseError : H.Error -> String
parseError error =
    case error of
        H.BadBody body ->
            "Encountered bad body error with body " ++ body

        _ ->
            "Encountered other error"


prevDay : Date -> Date
prevDay date =
    Date.add Days -1 date


nextDay : Date -> Date
nextDay date =
    Date.add Days 1 date


dateToString : Date -> String
dateToString date =
    format "EEE MMMM d y" date


newExerciseBody : String -> Data -> InsertExerciseRequest
newExerciseBody id data =
    { id = id
    , payload = 
        { exercise = createNew data.form
        , order = List.length <| OrderedDict.keys data.workout
        , day = weekday data.today
        }
    }


parseInsertResults : Result H.Error () -> Msg
parseInsertResults result =
    case result of
        Ok () ->
            ClearForm

        _ ->
            InsertError "Failed to insert exercise"



-- VIEW --


view : Model -> Html Msg
view model =
    case model of
        Unauthenticated ->
            div [] []

        Authenticated data ->
            let
                exerciseList =
                    data.workout
                        |> OrderedDict.map viewExercises
                        |> OrderedDict.values
            in
            div [ style "padding" "20px" ]
                [ div [ class "row" ]
                    [ div [ class "col-lg" ]
                        [ div [ class "container-fluid navbar" ]
                            [ button [ class "btn btn-outline-dark", onClick (Selected (prevDay data.today)) ] [ text "<" ]
                            , h2 [ class "mx-auto" ]
                                [ text (dateToString data.today) ]
                            , button [ class "btn btn-outline-dark", onClick (Selected (nextDay data.today)) ] [ text ">" ]
                            ]
                        , div [] exerciseList
                        , input [ type_ "checkbox", class "fake-checkbox", onCheck (\_ -> CreateFormToggled), checked (isEditorToggled data) ] []
                        , viewForm data.form
                        , div [ class "d-flex p-2" ]
                            [ button [ class "btn btn-outline-dark mx-auto bg-light", style "width" "90%", onClick CreateFormToggled ]
                                [ if isEditorToggled data then
                                    text "-"

                                  else
                                    text "+"
                                ]
                            ]
                        ]
                    ]
                ]


viewExercises : String -> StrengthExercise -> Html Msg
viewExercises id exercise =
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
            , onClick (Toggled id)
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
                    , button [ type_ "button", class "btn btn-outline-dark float-right", onClick (DeleteExercise id) ]
                        [ text "Delete!"
                        ]
                    ]
                ]
            ]
        , input [ type_ "checkbox", class "fake-checkbox", checked exercise.expanded, onCheck (\_ -> Toggled exercise.name) ] []
        , div [ class "slide" ] (List.indexedMap (viewSet id) exercise.sets)
        ]


viewSet : String -> Int -> StrengthSet -> Html Msg
viewSet id num set =
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
                [ button [ type_ "button", class "btn btn-outline-dark float-right", onClick (LogSet id set) ]
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
                    [ button [ class "btn btn-outline-dark mx-auto", style "margin-top" "30px", style "width" "50%", onClick ClickedCreateExercise ]
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
