module Pages.Workouts exposing (..)

import Api.Exercises as Exercise exposing (InsertPayload)
import Api.Supabase exposing (AuthenticatedUser, RequestError(..), key, url)
import Api.User as User exposing (storeUser)
import Browser.Navigation as Nav exposing (Key)
import Date exposing (Date, Unit(..), format, weekday)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, form, h2, h3, h4, h5, input, label, small, span, text)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, stopPropagationOn)
import Http as H
import Json.Decode
import Maybe exposing (withDefault)
import Set exposing (Set)
import StrengthSet exposing (StrengthExercise, StrengthSet, addLastSet, changeRepCountForExercise, changeWeightForExercise, removeSet)
import Swiper
import Task
import Time
import Url.Builder
import Utils.Log exposing (LogType(..), log, logCmd)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)
import Workout exposing (Workout)
import WorkoutCreator exposing (WorkoutCreator, createNew, emptyForm, newSetReps, newSetWeight, toggleCreator, updateName, updateNumSets)



-- MODEL --


type alias WorkoutState =
    { api : Exercise.API
    , currentUser : AuthenticatedUser
    , workout : OrderedDict String StrengthExercise
    , open : Set String
    , logged : Dict String (Set Int)
    , openMobile : Maybe String
    , workoutEditor : Maybe ( String, StrengthExercise )
    , form : WorkoutCreator
    , today : Date
    , navKey : Nav.Key
    , navarSwipeState : Swiper.SwipingState
    }


type Model
    = Unauthenticated
    | Authenticated WorkoutState


isEditorToggled : WorkoutState -> Bool
isEditorToggled data =
    data.form.isOpen


isLogged : WorkoutState -> String -> Int -> Bool
isLogged state id index =
    Dict.get id state.logged
        |> Maybe.map (Set.member index)
        |> Maybe.withDefault False



-- UPDATE --


noOp : a -> ( a, Cmd msg )
noOp a =
    ( a, Cmd.none )


type Msg
    = Setup SetupMessage
    | Select SelectionMessage
    | Edit WorkoutEditorMessage
    | CreateNew WorkoutCreatorFormMessage
    | Log LoggingMessage
    | NoOp


type SetupMessage
    = LoggedIn AuthenticatedUser Nav.Key (Maybe SelectionMessage)
    | FetchedWorkout Workout
    | FetchError RequestError
    | FailedRefresh RequestError Nav.Key


handleSetup : SetupMessage -> Model -> ( Model, Cmd Msg )
handleSetup msg model =
    case model of
        Unauthenticated ->
            case msg of
                LoggedIn user navKey thenSelect ->
                    let
                        exerciseApi =
                            Exercise.api url key user

                        authenticatedModel =
                            Authenticated
                                { api = exerciseApi
                                , currentUser = user
                                , workout = OrderedDict.empty
                                , logged = Dict.empty
                                , open = Set.empty
                                , openMobile = Nothing
                                , workoutEditor = Nothing
                                , form = emptyForm
                                , today = Date.fromCalendarDate 2022 Time.Jan 1
                                , navKey = navKey
                                , navarSwipeState = Swiper.initialSwipingState
                                }

                        handleDate =
                            Maybe.map (\sel -> handleSelect sel authenticatedModel) thenSelect
                    in
                    handleDate
                        |> withDefault ( authenticatedModel, Task.perform (\day -> Select (Selected day)) Date.today )
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, storeUser user ])

                _ ->
                    log Error "Cannot update data on unauthenticated workouts page" model

        Authenticated data ->
            case msg of
                LoggedIn user _ _ ->
                    ( Authenticated { data | currentUser = user }, Cmd.batch [ logCmd Info "refreshed user", storeUser user ] )

                FailedRefresh err _ ->
                    log Error ("error encountered" ++ parseError err) model

                FetchedWorkout workout ->
                    ( Authenticated { data | workout = workout }, Cmd.none )

                FetchError err ->
                    case err of
                        Http (H.BadStatus 401) ->
                            let
                                userApi =
                                    User.api url key
                            in
                            ( model
                            , userApi.refreshAuth data.currentUser.refreshToken
                                |> Task.attempt (parseLogin data.navKey)
                            )

                        _ ->
                            log Error ("error encountered" ++ parseError err) model


type SelectionMessage
    = Toggled String
    | Selected Date
    | LoadedUrl Date
    | Swiped Swiper.SwipeEvent
    | ImporoperSelection String


handleSelect : SelectionMessage -> Model -> ( Model, Cmd Msg )
handleSelect msg model =
    case model of
        Unauthenticated ->
            log Error "Must be logged in to select anything!" model

        Authenticated data ->
            case msg of
                Toggled id ->
                    let
                        isToggled =
                            Set.member id data.open

                        untoggle =
                            \ex -> { data | open = Set.remove ex data.open, openMobile = Nothing }

                        toggle =
                            \ex -> { data | open = Set.insert ex data.open, openMobile = Just id }
                    in
                    (if isToggled then
                        untoggle id

                     else
                        toggle id
                    )
                        |> Authenticated
                        |> noOp

                Selected day ->
                    ( Authenticated { data | today = day, workout = OrderedDict.empty, form = emptyForm }
                    , Task.attempt parseWorkout (data.api.getWorkout day)
                    )

                LoadedUrl day ->
                    ( model
                    , Nav.pushUrl data.navKey (Url.Builder.absolute [ "date", Date.toIsoString day ] [])
                    )

                Swiped event ->
                    let
                        ( _, swipedLeft ) =
                            Swiper.hasSwipedLeft event data.navarSwipeState

                        ( nextState, swipedRight ) =
                            Swiper.hasSwipedRight event data.navarSwipeState

                        updated =
                            Authenticated { data | navarSwipeState = nextState }

                        action =
                            if swipedRight then
                                Select <| LoadedUrl (nextDay data.today)

                            else if swipedLeft then
                                Select <| LoadedUrl (prevDay data.today)

                            else
                                NoOp
                    in
                    update action updated

                ImporoperSelection errMsg ->
                    log Error errMsg model


type WorkoutEditorMessage
    = OpenWorkoutEditor String
    | AddSetToWorkout
    | RemoveSetFromEditor Int
    | CloseWorkoutEditor
    | EditWorkoutSets String (List StrengthSet)
    | DeleteExercise String
    | ChangeRepCount Int Int
    | ChangeWeight Int Float


handleWorkoutEditor : WorkoutEditorMessage -> Model -> ( Model, Cmd Msg )
handleWorkoutEditor msg model =
    case model of
        Unauthenticated ->
            log Error "Must be logged in to edit exercises" model

        Authenticated data ->
            case msg of
                OpenWorkoutEditor exerciseID ->
                    Authenticated
                        { data | workoutEditor = Maybe.map (\ex -> ( exerciseID, ex )) (OrderedDict.get exerciseID data.workout) }
                        |> noOp

                AddSetToWorkout ->
                    Authenticated
                        { data | workoutEditor = Maybe.map (\editor -> ( Tuple.first editor, addLastSet (Tuple.second editor) )) data.workoutEditor }
                        |> noOp

                RemoveSetFromEditor index ->
                    Authenticated
                        { data | workoutEditor = Maybe.map (\editor -> ( Tuple.first editor, removeSet index (Tuple.second editor) )) data.workoutEditor }
                        |> noOp

                CloseWorkoutEditor ->
                    ( Authenticated { data | workoutEditor = Nothing }
                    , Task.attempt parseWorkout (data.api.getWorkout data.today)
                    )

                EditWorkoutSets exerciseID sets ->
                    let
                        edit =
                            data.api.editSets exerciseID sets
                    in
                    ( model, Task.attempt parseEditResult edit )

                DeleteExercise id ->
                    ( model
                    , Date.today
                        |> Task.andThen (data.api.deleteExercise id)
                        |> Task.attempt parseInsertResults
                    )

                ChangeRepCount index reps ->
                    Authenticated
                        { data | workoutEditor = Maybe.map (Tuple.mapSecond (changeRepCountForExercise index reps)) data.workoutEditor }
                        |> noOp

                ChangeWeight index weight ->
                    Authenticated
                        { data | workoutEditor = Maybe.map (Tuple.mapSecond (changeWeightForExercise index weight)) data.workoutEditor }
                        |> noOp


type WorkoutCreatorFormMessage
    = CreateFormToggled
    | SetNumberEntered Int
    | ChangedWorkoutName String
    | UpdatedSetWeight Int Float
    | UpdatedSetReps Int Int
    | InsertError String
    | CreateNewExercise
    | ClearForm


handleCreate : WorkoutCreatorFormMessage -> Model -> ( Model, Cmd Msg )
handleCreate msg model =
    case model of
        Unauthenticated ->
            log Error "Must be logged in to edit workouts" model

        Authenticated data ->
            case msg of
                CreateFormToggled ->
                    Authenticated
                        { data | form = toggleCreator data.form }
                        |> noOp

                SetNumberEntered set ->
                    Authenticated
                        { data | form = updateNumSets set data.form }
                        |> noOp

                ChangedWorkoutName name ->
                    Authenticated
                        { data | form = updateName name data.form }
                        |> noOp

                UpdatedSetWeight index weight ->
                    Authenticated
                        { data | form = newSetWeight index weight data.form }
                        |> noOp

                UpdatedSetReps index reps ->
                    Authenticated
                        { data | form = newSetReps index reps data.form }
                        |> noOp

                InsertError err ->
                    log Error ("Error while inserting exercise log " ++ err) model

                CreateNewExercise ->
                    ( model, Task.attempt parseInsertResults (data.api.insert (newExerciseBody data)) )

                ClearForm ->
                    ( Authenticated { data | form = emptyForm }
                    , Task.attempt parseWorkout (data.api.getWorkout data.today)
                    )


type LoggingMessage
    = LogSet String Int StrengthSet
    | LogSets String (List StrengthSet)
    | LoggedSet String Int
    | EditWeight String Int Float


handleLogging : LoggingMessage -> Model -> ( Model, Cmd Msg )
handleLogging msg model =
    case model of
        Unauthenticated ->
            log Error "Must be logged in to log exercisees" model

        Authenticated data ->
            case msg of
                LogSet id index _ ->
                    let
                        loggedIndices =
                            Dict.get id data.logged
                                |> Maybe.map (\set -> Set.insert index set)
                                |> Maybe.withDefault Set.empty
                    in
                    Authenticated { data | logged = Dict.insert id loggedIndices data.logged } |> noOp

                -- ( model, Task.attempt parseInsertResults (data.api.logSet id set) )
                LogSets id sets ->
                    ( model, Cmd.batch (List.indexedMap (\index set -> handleLogging (LogSet id index set) model) sets |> List.map Tuple.second) )

                LoggedSet _ _ ->
                    log Info "In the future we will grey out logged sets" model

                EditWeight id index weight ->
                    Authenticated
                        { data | workout = OrderedDict.update id (Maybe.map <| changeWeightForExercise index weight) data.workout }
                        |> noOp


updateWorkout : (Workout -> Workout) -> WorkoutState -> WorkoutState
updateWorkout mapper data =
    { data | workout = mapper data.workout }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Setup setupMsg ->
            handleSetup setupMsg model

        Select selection ->
            handleSelect selection model

        Edit edit ->
            handleWorkoutEditor edit model

        CreateNew exercise ->
            handleCreate exercise model

        Log log ->
            handleLogging log model

        NoOp ->
            ( model, Cmd.none )


parseWorkout : Result RequestError Workout -> Msg
parseWorkout result =
    case result of
        Ok workout ->
            FetchedWorkout workout |> Setup

        Err err ->
            FetchError err |> Setup


parseLogin : Key -> Result RequestError AuthenticatedUser -> Msg
parseLogin key result =
    case result of
        Ok user ->
            LoggedIn user key Nothing |> Setup

        Err err ->
            FailedRefresh err key |> Setup


parseError : RequestError -> String
parseError error =
    case error of
        Http (H.BadBody body) ->
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


newExerciseBody : WorkoutState -> InsertPayload
newExerciseBody data =
    { exercise = createNew data.form
    , order = List.length <| OrderedDict.keys data.workout
    , day = weekday data.today
    }


parseInsertResults : Result RequestError () -> Msg
parseInsertResults result =
    case result of
        Ok () ->
            ClearForm |> CreateNew

        _ ->
            InsertError "Failed to insert exercise" |> CreateNew


parseEditResult : Result RequestError () -> Msg
parseEditResult result =
    case result of
        Ok () ->
            CloseWorkoutEditor |> Edit

        _ ->
            InsertError "Failed to edit exercise" |> CreateNew



-- VIEW --


view : Model -> Html Msg
view model =
    case model of
        Unauthenticated ->
            div [] []

        Authenticated data ->
            let
                isToggled =
                    \exerciseId -> Set.member exerciseId data.open

                exerciseList =
                    data.workout
                        |> OrderedDict.map (viewExercises (isLogged data) isToggled)
                        |> OrderedDict.values
            in
            div [ class "workouts" ]
                [ Maybe.map
                    viewExerciseEditor
                    data.workoutEditor
                    |> Maybe.withDefault (div [] [])
                , div [ class "row" ]
                    [ div [ class "col-lg" ]
                        [ div ([ class "container-fluid navbar navbar-expand-lg navbar-light border rounded", style "margin-bottom" "3px" ] ++ Swiper.onSwipeEvents (\e -> Swiped e |> Select))
                            [ a [ class "btn btn-outline-dark", href ("/date/" ++ (prevDay data.today |> Date.toIsoString)) ] [ text "<" ]
                            , h2 [ class "mx-auto" ]
                                [ text (dateToString data.today) ]
                            , a [ class "btn btn-outline-dark", href ("/date/" ++ (nextDay data.today |> Date.toIsoString)) ] [ text ">" ]
                            ]
                        , div [] exerciseList
                        , input [ type_ "checkbox", class "fake-checkbox", onCheck (\_ -> CreateFormToggled |> CreateNew), checked (isEditorToggled data) ] []
                        , viewForm data.form
                        , div [ class "d-flex p-2" ]
                            [ button [ class "btn btn-outline-dark mx-auto bg-light", style "width" "90%", onClick (CreateFormToggled |> CreateNew) ]
                                [ if isEditorToggled data then
                                    text "-"

                                  else
                                    text "+"
                                ]
                            ]
                        ]
                    ]
                , case data.openMobile of
                    Just id ->
                        OrderedDict.get id data.workout
                            |> Maybe.map (viewSetModal (isLogged data) id)
                            |> Maybe.withDefault (div [] [])

                    Nothing ->
                        div [] []
                ]


viewExercises : (String -> Int -> Bool) -> (String -> Bool) -> String -> StrengthExercise -> Html Msg
viewExercises logged expanded id exercise =
    let
        ( weights, reps ) =
            getSetRanges exercise.sets
    in
    div [ class "container-fluid border border-5 rounded list-group", style "padding-right" "0px", style "margin-bottom" "2px" ]
        [ div
            [ class
                ("list-group-item bg-light border-5"
                    ++ (if expanded id then
                            ""

                        else
                            " rounded-bottom"
                       )
                )
            , onClick (Toggled id |> Select)
            ]
            [ div [ class "row justify-content-between" ]
                [ div [ class "col-sm-6" ]
                    [ h4 [ style "font-size-adjust" "0.3", style "width" "100%" ]
                        [ text exercise.name
                        ]
                    ]
                , div [ class "row col-sm-3 justify-content-between" ]
                    [ h4 []
                        [ text (String.fromInt (List.length exercise.sets))
                        , small [ style "font-size" "0.5em" ]
                            [ text "sets"
                            ]
                        ]
                    , div []
                        [ h4 []
                            [ text weights
                            , small [ style "font-size" "0.5em" ]
                                [ text "lbs"
                                ]
                            ]
                        ]
                    , div []
                        [ h4 []
                            [ text reps
                            , small [ style "font-size" "0.5em" ]
                                [ text "reps"
                                ]
                            ]
                        ]
                    ]
                , div [ class "col-sm-3" ]
                    [ div [ class "d-flex justify-content-end buttons" ]
                        [ button [ type_ "button", class "btn btn-outline-primary visible-large", overrideOnClickWith (LogSets id exercise.sets |> Log) ]
                            [ text "Log all!"
                            ]
                        , button [ style "margin-left" "5px", type_ "button", class "btn btn-outline-dark edit-button", overrideOnClickWith (OpenWorkoutEditor id |> Edit) ]
                            [ text "Edit"
                            ]
                        ]
                    ]
                ]
            ]
        , input [ type_ "checkbox", class "fake-checkbox", checked (expanded id), onCheck (\_ -> Toggled id |> Select) ] []
        , div [ class "slide" ] (List.indexedMap (viewSet logged id) exercise.sets)
        ]


disableDefault : msg -> ( msg, Bool )
disableDefault msg =
    ( msg, True )


overrideOnClickWith : Msg -> Attribute Msg
overrideOnClickWith msg =
    stopPropagationOn "click" (Json.Decode.map disableDefault (Json.Decode.succeed msg))


viewSet : (String -> Int -> Bool) -> String -> Int -> StrengthSet -> Html Msg
viewSet logged exerciseId num set =
    div [ class "container-fluid list-group-item bg-light border-5" ]
        [ div [ class "row justify-content-between", style "white-space" "nowrap" ]
            [ div [ class "d-flex justify-content-start" ]
                [ h2 [ class "visible-large", style "margin-top" ".5rem" ]
                    [ text (String.fromInt (num + 1) ++ ".")
                    ]
                , h4 [ class "visible-small", style "margin-top" ".5rem" ]
                    [ text (String.fromInt (num + 1) ++ ".")
                    ]
                ]
            , div [ class "row col-sm-6 justify-content-around", style "margin-top" ".5rem" ]
                [ div [ class "row justify-content-center" ]
                    [ h3 [ class "visible-large" ]
                        [ text (String.fromFloat set.weight)
                        , small [ style "font-size" "0.5em", style "margin-right" "2px" ] [ text "lbs" ]
                        ]
                    , h5 [ class "visible-small" ]
                        [ text (String.fromFloat set.weight)
                        , small [ style "font-size" "0.5em", style "margin-right" "2px" ] [ text "lbs" ]
                        ]
                    , div [ style "max-width" "70px", style "margin-left" "2px", id "weight" ]
                        [ input
                            [ type_ "number"
                            , class "form-control"
                            , value (String.fromFloat set.weight)
                            , disabled (logged exerciseId num)
                            , onInput
                                (\weight ->
                                    String.toFloat weight
                                        |> Maybe.withDefault 0.0
                                        |> EditWeight exerciseId num
                                        |> Log
                                )
                            ]
                            []
                        ]
                    ]
                , div [ class "row justify-content-center" ]
                    [ h3 [ class "visible-large", style "padding-right" "2px" ]
                        [ text (String.fromInt set.reps)
                        , small [ style "font-size" "0.5em" ]
                            [ text "reps"
                            ]
                        ]
                    , h5 [ class "visible-small", style "padding-right" "2px" ]
                        [ text (String.fromInt set.reps)
                        , small [ style "font-size" "0.5em" ]
                            [ text "reps"
                            ]
                        ]
                    , div [ style "max-width" "70px", style "margin-left" "2px" ]
                        [ input
                            [ type_ "number"
                            , class "form-control"
                            , value (String.fromInt set.reps)
                            , disabled (logged exerciseId num)
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "col-sm-2 d-flex justify-log-button", style "margin-top" ".5rem" ]
                [ button [ type_ "button", class "btn btn-outline-dark", overrideOnClickWith (LogSet exerciseId num set |> Log) ]
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
                        [ input [ class "form-control", placeholder "Name", onInput (\s -> ChangedWorkoutName s |> CreateNew), value form.name ] [] ]
                    ]
                , div [ class "container-fluid row" ]
                    [ div [ class "col-sm-2" ]
                        [ h4 [] [ text "Num sets: " ]
                        ]
                    , div [ class "col" ]
                        [ input
                            [ class "form-control"
                            , placeholder "Sets"
                            , type_ "number"
                            , value
                                (if form.numSets == 0 then
                                    ""

                                 else
                                    String.fromInt form.numSets
                                )
                            , onInput (\count -> SetNumberEntered (String.toInt count |> withDefault 0) |> CreateNew)
                            ]
                            []
                        ]
                    ]
                , div [ style "overflow" "scroll", style "max-height" "300px" ] [ viewSetForm form ]
                , div [ class "d-flex justify-content-center" ]
                    [ button [ class "btn btn-outline-dark mx-auto", style "margin-top" "30px", style "width" "50%", onClick (CreateNewExercise |> CreateNew) ]
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
            , input [ class "form-control", placeholder "Weight", type_ "number", onInput (\weight -> UpdatedSetWeight index (String.toFloat weight |> withDefault 0) |> CreateNew) ] []
            ]
        , div [ class "col-sm-4 d-flex flow-row" ]
            [ h4 [ style "margin-right" "10px" ] [ text "Reps: " ]
            , input [ class "form-control", placeholder "Reps", type_ "number", onInput (\reps -> UpdatedSetReps index (String.toInt reps |> withDefault 0) |> CreateNew) ] []
            ]
        ]


viewExerciseEditor : ( String, StrengthExercise ) -> Html Msg
viewExerciseEditor ( exerciseId, exercise ) =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "bottom" "0"
        , style "right" "0"
        , style "left" "0"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background-color" "rgba(33, 43, 54, 0.4)"
        , style "z-index" "1"
        , style "overflow-y" "scroll"
        , onClick (CloseWorkoutEditor |> Edit)
        ]
        [ div
            [ class "px-3"
            , style "border-style" "solid"
            , style "border-radius" "3px"
            , style "border-color" "white"
            , style "background-color" "white"
            , style "height" "500px"
            , style "width" "440px"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , overrideOnClickWith NoOp
            ]
            [ h2 [ style "margin-bottom" "15px", style "margin-top" "15px" ]
                [ text ("Edit " ++ exercise.name)
                ]
            , div
                [ class "container mx-auto"
                , style "justify-content" "center"
                , style "padding" "25px"
                , style "overflow" "scroll"
                ]
                (List.indexedMap viewEditFormLine exercise.sets)
            , div [ class "d-flex", style "width" "100%", style "margin-top" "15px", style "margin-bottom" "15px" ]
                [ button [ class "btn btn-outline-dark mx-auto bg-light", style "width" "90%", overrideOnClickWith (AddSetToWorkout |> Edit) ]
                    [ text "Add set"
                    ]
                ]
            , div [ class "d-flex justify-content-between mt-auto", style "margin-bottom" "15px", style "width" "50%" ]
                [ button [ class "btn btn-outline-danger", overrideOnClickWith (DeleteExercise exerciseId |> Edit) ] [ text "Delete" ]
                , button [ class "btn btn-outline-primary", overrideOnClickWith (EditWorkoutSets exerciseId exercise.sets |> Edit) ] [ text "Submit" ]
                ]
            ]
        ]


viewEditFormLine : Int -> StrengthSet -> Html Msg
viewEditFormLine index set =
    div [ class "row mx-auto justify-content-between form-inline", style "margin-bottom" "5px" ]
        [ label [ for "reps-editor", class "fs-1 fw-bold" ]
            [ span [ class "fs-1 fw-bold" ]
                [ text (String.fromInt (index + 1) ++ ".") ]
            ]
        , div [ class "row" ]
            [ input
                [ id "reps-editor"
                , class "form-control"
                , style "margin-right" "7px"
                , style "width" "75px"
                , type_ "number"
                , value (String.fromInt set.reps)
                , onInput
                    (\count ->
                        String.toInt count
                            |> Maybe.withDefault 0
                            |> ChangeRepCount index
                            |> Edit
                    )
                ]
                []
            , label [ for "reps-editor" ] [ text "reps" ]
            ]
        , div [ class "row" ]
            [ input
                [ class "form-control"
                , style "margin-right" "7px"
                , style "width" "75px"
                , type_ "number"
                , value (String.fromFloat set.weight)
                , onInput
                    (\weight ->
                        String.toFloat weight
                            |> Maybe.withDefault 0
                            |> ChangeWeight index
                            |> Edit
                    )
                ]
                []
            , label [ for "reps-editor" ] [ text "lbs" ]
            ]
        , div [ class "row" ]
            [ button [ class "btn btn-outline-danger", type_ "button", overrideOnClickWith (RemoveSetFromEditor index |> Edit) ] [ text "Remove" ]
            ]
        ]


viewSetModal : (String -> Int -> Bool) -> String -> StrengthExercise -> Html Msg
viewSetModal logged id exercise =
    div
        [ class "view-set-modal"
        , style "position" "fixed"
        , style "top" "0"
        , style "bottom" "0"
        , style "right" "0"
        , style "left" "0"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background-color" "rgba(33, 43, 54, 0.4)"
        , onClick (Toggled id |> Select)
        ]
        [ div
            [ class "px-3"
            , style "border-style" "solid"
            , style "border-radius" "3px"
            , style "border-color" "white"
            , style "background-color" "white"
            , style "height" "70%"
            , style "width" "80%"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "overflow" "scroll"
            , overrideOnClickWith NoOp
            ]
            [ h2 [ style "text-align" "center" ]
                [ text exercise.name
                ]
            , div [] (List.indexedMap (viewSet logged id) exercise.sets)
            , button [ type_ "button", class "btn btn-outline-primary mt-auto mb-2", overrideOnClickWith (LogSets id exercise.sets |> Log) ]
                [ text "Log all!"
                ]
            ]
        ]
