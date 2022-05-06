module Pages.Workouts exposing (..)

import Api.Exercises as Exercise exposing (InsertPayload)
import Api.Supabase exposing (AuthenticatedUser, RequestError(..), key, url)
import Api.User as User exposing (storeUser)
import Browser.Navigation as Nav exposing (Key)
import Date exposing (Date, Unit(..), format, weekday)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, form, h2, h3, h4, h5, input, label, p, small, span, text)
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
import Url.Builder exposing (string)
import Utils.Log exposing (LogType(..), log, logCmd)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)
import Workout exposing (Workout)
import WorkoutCreator exposing (WorkoutCreator, createNew, emptyForm, newSetReps, newSetWeight, toggleCreator, updateName, updateNumSets)



-- MODEL --


type alias WorkoutState =
    { api : Exercise.API
    , currentUser : AuthenticatedUser
    , workout : OrderedDict String StrengthExercise
    , lastWeek : OrderedDict String StrengthExercise
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
                                , lastWeek = OrderedDict.empty
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
                        |> withDefault ( authenticatedModel, Task.perform (\day -> Select (LoadedUrl day)) Date.today )
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
                            ( Unauthenticated
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
                    , Nav.pushUrl data.navKey (makeExerciseUrl day)
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
    format "EEE, MMMM d" date


makeExerciseUrl : Date -> String
makeExerciseUrl date =
    Url.Builder.absolute [ "workout" ] [ string "date" (Date.toIsoString date) ]


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
            div [ class "flex justify-center w-full bg-gray-900 sm:px-3" ]
                [ Maybe.map
                    viewExerciseEditor
                    data.workoutEditor
                    |> Maybe.withDefault (div [] [])
                , div [ class "w-screen text-blue-200" ]
                    [ div []
                        [ div (class "flex flex-row justify-between border-b-2 border-blue-400 mb-3 p-2 pb-2" :: Swiper.onSwipeEvents (\e -> Swiped e |> Select))
                            [ a [ class "invisible sm:visible my-auto hover:text-blue-400", href (prevDay data.today |> makeExerciseUrl) ] [ text "< yesterday" ]
                            , h2 [ class "text-4xl text-center" ]
                                [ text (dateToString data.today) ]
                            , a [ class "invisible sm:visible my-auto hover:text-blue-400", href (nextDay data.today |> makeExerciseUrl) ] [ text "tomorrow >" ]
                            ]
                        , div [] exerciseList
                        , input [ type_ "checkbox", class "opacity-0 h-0 absolute", onCheck (\_ -> CreateFormToggled |> CreateNew), checked (isEditorToggled data) ] []
                        , div
                            [ class
                                (if isEditorToggled data then
                                    "visible"

                                 else
                                    "hidden"
                                )
                            ]
                            [ viewForm data.form ]
                        , div [ class "flex justify-center" ]
                            [ button [ class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400 w-11/12", onClick (CreateFormToggled |> CreateNew) ]
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
                        div [] []

                    -- OrderedDict.get id data.workout
                    --     |> Maybe.map (viewSetModal (isLogged data) id)
                    --     |> Maybe.withDefault (div [] [])
                    Nothing ->
                        div [] []
                ]


viewExercises : (String -> Int -> Bool) -> (String -> Bool) -> String -> StrengthExercise -> Html Msg
viewExercises logged expanded id exercise =
    let
        ( weights, reps ) =
            getSetRanges exercise.sets
    in
    div [ class "border rounded-md border-blue-400 mb-1" ]
        [ div
            [ class
                ("cursor-pointer flex flex-row justify-between py-2 px-2"
                    ++ (if expanded id then
                            " border-b border-blue-400"

                        else
                            ""
                       )
                )
            , onClick (Toggled id |> Select)
            ]
            [ div [ class "w-56 text-xl content-center my-auto" ]
                [ text exercise.name
                ]
            , div [ class "flex flex-row w-48 justify-between my-auto" ]
                [ div []
                    [ span [ class "text-xl" ]
                        [ text (String.fromInt (List.length exercise.sets))
                        ]
                    , span [ class "text-xs" ]
                        [ text "sets"
                        ]
                    ]
                , div []
                    [ span [ class "text-xl" ]
                        [ text weights ]
                    , span [ class "text-xs" ]
                        [ text "lbs"
                        ]
                    ]
                , div []
                    [ span [ class "text-xl" ]
                        [ text reps ]
                    , span [ class "text-xs" ]
                        [ text "reps"
                        ]
                    ]
                ]
            , div []
                [ div [ class "flex flex-row" ]
                    [ button [ type_ "button", class "border-2 border-red-400 w-24 rounded-md m-2 p-2 hover:bg-red-400 sm:block hidden", overrideOnClickWith (OpenWorkoutEditor id |> Edit) ]
                        [ text "Edit"
                        ]
                    , button [ type_ "button", class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400 sm:block hidden", overrideOnClickWith (LogSets id exercise.sets |> Log) ]
                        [ text "Log all!"
                        ]
                    ]
                ]
            ]
        , input [ type_ "checkbox", class "opacity-0 h-0 absolute", checked (expanded id), onCheck (\_ -> Toggled id |> Select) ] []
        , div
            [ class
                (if expanded id then
                    "transition-all ease-in-out duration-700 clear-both"

                 else
                    "hidden overflow-hidden transition-all ease-in-out duration-700 clear-both"
                )
            ]
            (List.indexedMap (viewSet logged id) exercise.sets)
        ]


disableDefault : msg -> ( msg, Bool )
disableDefault msg =
    ( msg, True )


overrideOnClickWith : Msg -> Attribute Msg
overrideOnClickWith msg =
    stopPropagationOn "click" (Json.Decode.map disableDefault (Json.Decode.succeed msg))


viewSet : (String -> Int -> Bool) -> String -> Int -> StrengthSet -> Html Msg
viewSet logged exerciseId num set =
    div [ class "flex flex-row border-b border-blue-400 justify-between py-auto px-2" ]
        [ div [ class "d-flex justify-center my-auto mr-3" ]
            [ h2 [ class "text-xl" ]
                [ text (String.fromInt (num + 1) ++ ".")
                ]
            ]
        , div [ class "flex flex-row justify-between my-auto w-auto" ]
            [ div [ class "flex flex-row justify-between sm:mr-10 mr-5" ]
                [ h2 [ class "text-xl" ]
                    [ text (String.fromFloat set.weight)
                    , span [ class "text-xs" ] [ text "lbs" ]
                    ]
                , div [ class "ml-3" ]
                    [ input
                        [ type_ "number"
                        , class "w-16 border rounded-md bg-blue-100 text-black"
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
            , div [ class "flex flex-row justify-between mr-3" ]
                [ h2 [ class "text-xl" ]
                    [ text (String.fromInt set.reps)
                    , span [ class "text-xs" ]
                        [ text "reps"
                        ]
                    ]
                , div [ class "ml-3" ]
                    [ input
                        [ type_ "number"
                        , class "w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromInt set.reps)
                        , disabled (logged exerciseId num)
                        ]
                        []
                    ]
                ]
            ]
        , div []
            [ button [ type_ "button", class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400", overrideOnClickWith (LogSet exerciseId num set |> Log) ]
                [ text "Log set"
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
    div []
        [ div [ class "flex flex-col border border-blue-400 w-24 rounded-md w-full" ]
            [ div [ class "flex sm:flex-row flex-col sm:justify-around justify-center sm:h-16 h-24 pl-3 sm:pl-0"]
                [ div [ class "flex flex-row my-auto" ]
                    [ div []
                        [ h2 [ class "text-xl mr-4" ] [ text "Name: " ]
                        ]
                    , div []
                        [ input
                            [ class "w-80 border rounded-md bg-blue-100 text-black"
                            , placeholder "Name"
                            , onInput (\s -> ChangedWorkoutName s |> CreateNew)
                            , value form.name
                            ]
                            []
                        ]
                    ]
                , div [ class "flex flex-row my-auto" ]
                    [ div [ ]
                        [ h4 [ class "text-xl mr-4"] [ text "Num sets: " ]
                        ]
                    , div []
                        [ input
                            [ class "w-80 border rounded-md bg-blue-100 text-black"
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
                ]
            , div [ class "h-80 overflow-scroll" ] [ viewSetForm form ]
            , div [ class "flex justify-center" ]
                [ button [ class "border-2 border-blue-400 w-30 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400", onClick (CreateNewExercise |> CreateNew) ]
                    [ text "Create set!" ]
                ]
            ]
        ]


viewSetForm : WorkoutCreator -> Html Msg
viewSetForm form =
    div [] (List.range 1 form.numSets |> List.map viewFormSingleSet)


viewFormSingleSet : Int -> Html Msg
viewFormSingleSet index =
    div [ class "flex flex-row justify-between px-6 pb-3" ]
        [ div [ ]
            [ h4 [class "text-lg"] [ text (String.fromInt index ++ ".") ]
            ]
        , div [ class "flex flow-row" ]
            [ h4 [ class "text-lg pr-2" ]
                [ text "Starting weight: " ]
            , input [ class "border rounded-md bg-blue-100 text-black pr-3", placeholder "Weight", type_ "number", onInput (\weight -> UpdatedSetWeight index (String.toFloat weight |> withDefault 0) |> CreateNew) ] []
            ]
        , div [ class "flex flow-row" ]
            [ h4 [ class "text-lg pr-2" ] [ text "Reps: " ]
            , input [ class "border rounded-md bg-blue-100 text-black pr-3", placeholder "Reps", type_ "number", onInput (\reps -> UpdatedSetReps index (String.toInt reps |> withDefault 0) |> CreateNew) ] []
            ]
        ]


viewExerciseEditor : ( String, StrengthExercise ) -> Html Msg
viewExerciseEditor ( exerciseId, exercise ) =
    div
        [ class "fixed inset-0 flex items-center justify-center bg-gray-800 bg-opacity-50 overflow-y-scroll z-10"
        , onClick (CloseWorkoutEditor |> Edit)
        ]
        [ div
            [ class "px-3 border-2 border-blue-400 rounded-md bg-blue-900 h-auto w-auto flex flex-col items-center text-blue-200"
            , overrideOnClickWith NoOp
            ]
            [ h2 [ class "text-3xl px-5 pt-2 text-blue-200" ]
                [ text ("Edit " ++ exercise.name)
                ]
            , div
                [ class "flex flex-col justify-center overflow-scroll w-full p-3 mx-auto"
                ]
                (List.indexedMap viewEditFormLine exercise.sets)
            , div [ class "flex flex-row justify-center w-6/12" ]
                [ button [ class "border-2 border-blue-400 w-full rounded-md mt-1 mb-3 p-2 hover:bg-blue-400", overrideOnClickWith (AddSetToWorkout |> Edit) ]
                    [ text "Add set"
                    ]
                ]
            , div [ class "flex flex-row justify-between w-6/12" ]
                [ button [ class "border-2 border-red-400 rounded-md mt-1 mb-3 p-2 hover:bg-red-400 w-5/12", overrideOnClickWith (DeleteExercise exerciseId |> Edit) ] [ text "Delete" ]
                , button [ class "border-2 border-blue-400 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400 w-5/12", overrideOnClickWith (EditWorkoutSets exerciseId exercise.sets |> Edit) ] [ text "Submit" ]
                ]
            ]
        ]


viewEditFormLine : Int -> StrengthSet -> Html Msg
viewEditFormLine index set =
    div [ class "flex flex-row my-2 h-11" ]
        [ div [  class "pr-2" ]
            [ span [ ]
                [ text (String.fromInt (index + 1) ++ ".") ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ id "reps-editor"
                , class "border rounded-md bg-blue-100 text-black pr-3"
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
            , label [ class "text-md" ,for ( "reps-editor-" ++( String.fromInt index)) ] [ text "reps" ]
            ]
        , div [ class "flex flex-row pr-3" ]
            [ input
                [ class "border rounded-md bg-blue-100 text-black pr-3"
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
            , label [ class "text-md", for ( "weight-editor-" ++( String.fromInt index)) ] [ text "lbs" ]
            ]
        , div [ class "flex items-center" ]
            [ button [ class "border-2 border-red-400 w-30 rounded-md hover:bg-red-400", type_ "button", overrideOnClickWith (RemoveSetFromEditor index |> Edit) ] [ text "Remove" ]
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
