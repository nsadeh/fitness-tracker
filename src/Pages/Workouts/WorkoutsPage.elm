module Pages.Workouts.WorkoutsPage exposing (..)

import Api.Supabase exposing (AuthenticatedUser, key, url)
import Api.User as User exposing (storeUser)
import Array
import Browser.Navigation as Nav
import Date
import Dict
import Html exposing (Html, a, button, div, h2, input, text)
import Html.Attributes exposing (checked, class, href, type_)
import Html.Events exposing (onCheck, onClick)
import Http as H
import Pages.Workouts.ExerciseBuilder as Builder
import Pages.Workouts.ExerciseEditor as Editor
import Pages.Workouts.ExercisePageNavigation as Navigation
import Pages.Workouts.ExerciseView exposing (viewExercise)
import Pages.Workouts.Utils exposing (dateToString, nextDay, prevDay)
import Pages.Workouts.WorkoutLogger as WorkoutLogger exposing (Msg(..))
import Pages.Workouts.WorkoutsState exposing (NavbarSwipeDirection(..), WorkoutsPageState, addToSwapState, emptyState, formatDateWorkoutURL, isExposedEditButton, isLoggedOn, isToggled, removeFromSwapState, swapState, swappable, updateBuilder, updateEditor, updateExercise, updateLog, updateWorkout)
import Set
import StrengthSet exposing (LoggedStrengthExercise, asExercise)
import Swiper
import Task
import Utils.Error as Error exposing (RequestError(..))
import Utils.Log exposing (LogType(..), log)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)
import Pages.Workouts.WorkoutsState exposing (removeExercise)


type Model
    = Unauthenticated
    | Authenticated WorkoutsPageState
    | Loading WorkoutsPageState


type FetchAction
    = Workout (Task.Task RequestError (OrderedDict String LoggedStrengthExercise))
    | Exercise String (Task.Task RequestError (Maybe LoggedStrengthExercise))
    | User Nav.Key (Maybe Navigation.Action) (Task.Task RequestError AuthenticatedUser)


type Msg
    = Editor Editor.EditorMessage
    | FetchedWorkout (Result RequestError (OrderedDict String LoggedStrengthExercise))
    | FetchedExercise String (Result RequestError (Maybe LoggedStrengthExercise))
    | FetchedUser Nav.Key (Maybe Navigation.Action) (Result RequestError AuthenticatedUser)
    | Builder Builder.Msg
    | LogWorkout WorkoutLogger.Msg
    | Navigate Navigation.Action
    | SwapExerciseOrder String String
    | AddToSwap String
    | RemoveFromSwap String
    | ClearSwap
    | Fetch FetchAction
    | NoOp


doFetch : FetchAction -> Cmd Msg
doFetch action =
    case action of
        Workout getWorkout ->
            Task.attempt FetchedWorkout getWorkout

        User key next task ->
            Task.attempt (FetchedUser key next) task

        Exercise id getExercise ->
            Task.attempt (FetchedExercise id) getExercise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unauthenticated ->
            case msg of
                FetchedUser navKey nextAction result ->
                    case result of
                        Err err ->
                            log Error ("Error fetching user: \n" ++ Error.toString err) model

                        Ok user ->
                            let
                                ( page, action ) =
                                    loadWorkoutsData (emptyState user navKey) nextAction
                            in
                            ( Loading page, Cmd.batch [ action, storeUser user ] )

                _ ->
                    log Error "Must be logged in to do anything" model

        Authenticated page ->
            case msg of
                Editor edit ->
                    let
                        getExercise =
                            \id ->
                                OrderedDict.get id page.workout
                                    |> Maybe.map asExercise

                        editExercise =
                            \id exercise ->
                                Task.sequence [ page.api.editName page.today id exercise.name, page.api.editSets page.today id (Array.toList exercise.sets) ]
                                    |> Task.attempt (Error.respond (\_ -> Editor.ClosedEditor) (\_ -> Editor.DoNothing))
                                    |> Cmd.map Editor

                        deleteExercise =
                            \id ->
                                page.api.deleteExercise page.today id
                                    |> Task.attempt (Error.respond (\_ -> Editor.ClosedEditor) (\_ -> Editor.DoNothing))
                                    |> Cmd.map Editor

                        ( edits, nextEdits ) =
                            Editor.update
                                { getExercise = getExercise
                                , editExercise = editExercise
                                , deleteExercise = deleteExercise
                                , refreshExercise = refreshExercise page
                                }
                                edit
                                page.editor
                    in
                    ( Authenticated <| updateEditor page edits, nextEdits )

                FetchedWorkout result ->
                    case result of
                        Err error ->
                            case error of
                                Http (H.BadStatus 401) ->
                                    let
                                        user =
                                            User.api url key
                                    in
                                    ( Unauthenticated
                                    , user.refreshAuth page.user.refreshToken
                                        |> Task.attempt (FetchedUser page.navKey (Just <| Navigation.LoadURL page.today))
                                    )

                                _ ->
                                    log Error ("Encountered error fetching workout: \n" ++ Error.toString error) model

                        Ok workout ->
                            ( Authenticated <| updateWorkout page workout, Cmd.none )

                Builder build ->
                    let
                        createNew =
                            \exercise ->
                                page.api.insert page.today
                                    { exercise = exercise
                                    , order = List.length <| OrderedDict.keys page.workout
                                    , day = Date.weekday page.today
                                    }
                                    |> Task.attempt (Error.respond (\_ -> Builder.Cleared) (\_ -> Builder.Invalid))
                                    |> Cmd.map Builder
                    in
                    Builder.update { submit = createNew, refresh = page.api.getLoggedWorkouts page.today |> Workout |> doFetch } build page.creator
                        |> Tuple.mapFirst (updateBuilder page)
                        |> Tuple.mapFirst Authenticated

                FetchedUser navKey navAction result ->
                    case result of
                        Err error ->
                            log Error ("Fetch Error: " ++ Error.toString error) Unauthenticated

                        Ok user ->
                            let
                                ( updatedState, nextAction ) =
                                    loadWorkoutsData (emptyState user navKey) navAction
                            in
                            ( Authenticated updatedState, Cmd.batch [ nextAction, storeUser user ] )

                Navigate directive ->
                    loadWorkoutsData page (Just directive)
                        |> Tuple.mapFirst Authenticated

                LogWorkout logMsg ->
                    let
                        log =
                            \id sets ->
                                page.api.logExercise page.today id sets
                                    |> Task.attempt (Error.respond (\_ -> WorkoutLogger.LoggedWorkout id) (\_ -> WorkoutLogger.FailedToLog))
                                    |> Cmd.map LogWorkout
                    in
                    WorkoutLogger.update { log = log, refresh = refreshExercise page } logMsg page.log
                        |> Tuple.mapFirst (updateLog page)
                        |> Tuple.mapFirst Authenticated

                NoOp ->
                    log Info "Nothing todo" model

                Fetch action ->
                    ( Loading page, doFetch action )

                FetchedExercise id result ->
                    case result of
                        Err err ->
                            log Error (Error.toString err) model

                        Ok exercise ->
                            ( Maybe.map (updateExercise page id) exercise |> Maybe.withDefault (removeExercise page id) |> Authenticated, Cmd.none )

                SwapExerciseOrder id1 id2 ->
                    OrderedDict.swap id1 id2 page.workout
                        |> updateWorkout page
                        |> (\m ->
                                { m | swapState = Set.empty }
                                    |> Authenticated
                                    |> (\s -> ( s, Task.attempt (\_ -> NoOp) (page.api.swapExercises page.today id1 id2) ))
                           )

                AddToSwap id ->
                    if Set.size page.swapState >= 2 then
                        ( model, Cmd.none )

                    else
                        ( addToSwapState page id |> Authenticated, Cmd.none )

                RemoveFromSwap id ->
                    ( removeFromSwapState page id |> Authenticated, Cmd.none )

                ClearSwap ->
                    ( { page | swapState = Set.empty, exposedEditButton = Set.empty } |> Authenticated, Cmd.none )

        Loading page ->
            update msg (Authenticated page)


refreshExercise : WorkoutsPageState -> String -> Cmd Msg
refreshExercise page id =
    Exercise id (page.api.getExercise page.today id) |> doFetch


loadWorkoutsData : WorkoutsPageState -> Maybe Navigation.Action -> ( WorkoutsPageState, Cmd Msg )
loadWorkoutsData state directive =
    case directive of
        Just action ->
            Navigation.navigatePage { parseWorkout = \task -> Workout task |> doFetch, openEditor = \id -> Task.succeed (Editor.Opened id) |> Task.perform Editor } action state

        Nothing ->
            ( state, Task.perform (\date -> Navigation.LoadURL date |> Navigate) Date.today )


openWorkoutEditor : String -> Msg
openWorkoutEditor id =
    Editor.Opened id |> Editor


expand : String -> Msg
expand id =
    Navigation.ExpandExercise id |> Navigate


inputWeight : String -> Int -> String -> Msg
inputWeight id setNumber weightString =
    WeightRecorded id setNumber weightString |> LogWorkout


inputReps : String -> Int -> String -> Msg
inputReps id setNumber repsString =
    RepsRecorded id setNumber repsString |> LogWorkout


logWorkout : String -> Msg
logWorkout id =
    LogWorkout <| WorkoutLogger.RequestLogWorkout id


reopenLog : String -> Msg
reopenLog id =
    LogWorkout <| WorkoutLogger.Relogging id


view : Model -> Html Msg
view model =
    case model of
        Unauthenticated ->
            div [ class "min-h-screen justify-center w-full bg-gray-900 py-50" ]
                [ h2 [ class "text-lg" ]
                    [ text "Authentication issue!"
                    ]
                ]

        Authenticated state ->
            let
                exercisesState =
                    { expanded = isToggled state
                    , isLoggedToday = isLoggedOn state.today state
                    , getEnteredData =
                        \id setNumber ->
                            Dict.get id state.log
                                |> Maybe.map (\d -> d.draft)
                                |> Maybe.andThen (Array.get setNumber)
                    , isEditorOpen = isExposedEditButton state
                    , swapState = swapState state
                    , isChecked = \id -> Set.member id state.swapState
                    }

                api =
                    { onOpenWorkoutEditor = openWorkoutEditor
                    , onToggle = expand
                    , onWeightInput = inputWeight
                    , onRepsInput = inputReps
                    , onLogAction = logWorkout
                    , onRelogAction = reopenLog
                    , onSwipeExercise = \id event -> Navigation.SwipedExercise id event |> Navigate
                    , addToSwap = AddToSwap
                    , removeFromSwap = RemoveFromSwap
                    , swapExercises = \a b -> SwapExerciseOrder a b
                    }

                exercises =
                    state.workout
                        |> OrderedDict.map (viewExercise exercisesState api)
                        |> OrderedDict.values
            in
            div [ class "flex min-h-screen justify-center w-full bg-gray-900 sm:px-3 " ]
                [ Editor.view state.editor |> Html.map Editor
                , div [ class "w-screen text-blue-200" ]
                    [ div []
                        [ div (class "flex flex-row sm:justify-between justify-center border-b-2 border-blue-400 mb-3 p-2 pb-2" :: Swiper.onSwipeEvents (\e -> Navigation.SwipedNavbar e |> Navigate))
                            [ a [ class "hidden sm:block my-auto hover:text-blue-400", href (prevDay state.today |> Pages.Workouts.WorkoutsState.formatDateWorkoutURL) ] [ text "< yesterday" ]
                            , h2 [ class "text-4xl text-center" ]
                                [ text (dateToString state.today) ]
                            , a [ class "hidden sm:block my-auto hover:text-blue-400", href (nextDay state.today |> formatDateWorkoutURL) ] [ text "tomorrow >" ]
                            ]
                        , div [ class "overflow-y-scroll sm:mx-0 mx-1" ] exercises
                        , input [ type_ "checkbox", class "opacity-0 h-0 absolute", onCheck (\_ -> Builder.Opened), checked state.creator.isOpen ] [] |> Html.map Builder
                        , div
                            [ class
                                (if state.creator.isOpen then
                                    "visible"

                                 else
                                    "hidden"
                                )
                            ]
                            [ Builder.view state.creator ]
                            |> Html.map Builder
                        , swapButton (swapState state) state
                        , div [ class "flex justify-center" ]
                            [ button
                                [ class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400 w-11/12"
                                , onClick
                                    (if state.creator.isOpen then
                                        Builder.Closed

                                     else
                                        Builder.Opened
                                    )
                                ]
                                [ if state.creator.isOpen then
                                    text "-"

                                  else
                                    text "+"
                                ]
                            ]
                            |> Html.map Builder
                        ]
                    ]
                ]

        Loading _ ->
            div [ class "min-h-screen justify-center w-full bg-gray-900 py-50" ]
                [ h2 [ class "text-lg" ]
                    [ text "Loading placeholder!!"
                    ]
                ]


swapButton : Bool -> WorkoutsPageState -> Html Msg
swapButton swapState state =
    Maybe.map
        (\t ->
            div [ class "flex justify-center" ]
                [ button [ class "bg-blue-500 rounded-md", onClick (SwapExerciseOrder (Tuple.first t) (Tuple.second t)) ] [ text "Swap" ]
                ]
        )
        (swappable state)
        |> Maybe.withDefault
            (if swapState then
                div [ class "flex justify-center" ]
                    [ button [ class "bg-red-500 rounded-md", onClick ClearSwap ] [ text "Exit Swap" ]
                    ]

             else
                div [] []
            )
