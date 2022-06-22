module Pages.Workouts.WorkoutsPage exposing (..)

import Api.Supabase exposing (AuthenticatedUser, key, url)
import Api.User as User exposing (storeUser)
import Array
import Browser.Navigation as Nav
import Date
import Html exposing (Html, a, button, div, h2, input, text)
import Html.Attributes exposing (checked, class, href, type_)
import Html.Events exposing (onCheck, onClick)
import Http as H
import Pages.Workouts.ExerciseBuilder as Builder
import Pages.Workouts.ExerciseEditor as Editor
import Pages.Workouts.ExercisePageNavigation as Navigation
import Pages.Workouts.ExerciseView exposing (viewExercise)
import Pages.Workouts.Utils exposing (dateToString, nextDay, prevDay)
import Pages.Workouts.WorkoutsState exposing (NavbarSwipeDirection(..), WorkoutsPageState, emptyState, formatDateWorkoutURL, isToggled, updateBuilder, updateEditor, updateWorkout)
import StrengthSet exposing (LoggedStrengthExercise, asExercise)
import Swiper
import Task
import Utils.Error as Error exposing (RequestError(..))
import Utils.Log exposing (LogType(..), log)
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)


type Model
    = Unauthenticated
    | Authenticated WorkoutsPageState


type Msg
    = Editor Editor.EditorMessage
    | FetchedWorkout (Result RequestError (OrderedDict String LoggedStrengthExercise))
    | Builder Builder.Msg
    | FetchedUser Nav.Key (Maybe Navigation.Action) (Result RequestError AuthenticatedUser)
    | Navigate Navigation.Action
    | NoOp


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
                            ( Authenticated page, Cmd.batch [ action, storeUser user ] )

                _ ->
                    log Error "Must be logged in to do anything" model

        Authenticated page ->
            case msg of
                Editor edit ->
                    let
                        getExercise =
                            \id -> OrderedDict.get id page.workout |> Maybe.map asExercise

                        editExercise =
                            \id exercise ->
                                Task.sequence [ page.api.editName id exercise.name, page.api.editSets id (Array.toList exercise.sets) ]
                                    |> Task.attempt (Error.respond (\_ -> Editor.ClosedEditor) (\_ -> Editor.DoNothing))

                        deleteExercise =
                            \id ->
                                page.api.deleteExercise id page.today
                                    |> Task.attempt (Error.respond (\_ -> Editor.ClosedEditor) (\_ -> Editor.DoNothing))

                        ( edits, nextEdits ) =
                            Editor.update
                                { getExercise = getExercise
                                , editExercise = editExercise
                                , deleteExercise = deleteExercise
                                }
                                edit
                                page.editor
                    in
                    ( Authenticated <| updateEditor page edits, Cmd.map Editor nextEdits )

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
                                page.api.insert
                                    { exercise = exercise
                                    , order = List.length <| OrderedDict.keys page.workout
                                    , day = Date.weekday page.today
                                    }
                                    |> Task.attempt (Error.respond (\_ -> Builder.Cleared) (\_ -> Builder.Invalid))
                    in
                    Builder.update { submit = createNew } build page.creator
                        |> Tuple.mapBoth (updateBuilder page) (Cmd.map Builder)
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

                NoOp ->
                    Debug.todo "branch 'NoOp' not implemented"


loadWorkoutsData : WorkoutsPageState -> Maybe Navigation.Action -> ( WorkoutsPageState, Cmd Msg )
loadWorkoutsData state directive =
    case directive of
        Just action ->
            Navigation.navigatePage { parseWorkout = FetchedWorkout } action state

        Nothing ->
            ( state, Task.perform (\date -> Navigation.LoadURL date |> Navigate) Date.today )


openWorkoutEditor : String -> Msg
openWorkoutEditor id =
    Editor.Opened id |> Editor


expand : String -> Msg
expand id =
    Navigation.ExpandExercise id |> Navigate


inputWeight : String -> String -> Msg
inputWeight _ _ =
    NoOp


inputReps : String -> String -> Msg
inputReps _ _ =
    NoOp


view : Model -> Html Msg
view model =
    case model of
        Unauthenticated ->
            div [] []

        Authenticated state ->
            let
                exercisesState =
                    { expanded = isToggled state, isLogged = \_ -> False }

                api =
                    { onOpenWorkoutEditor = openWorkoutEditor
                    , onToggle = expand
                    , onWeightInput = inputWeight
                    , onRepsInput = inputReps
                    }

                exercises =
                    state.workout
                        |> OrderedDict.map (viewExercise exercisesState api)
                        |> OrderedDict.values
            in
            div [ class "flex justify-center w-full bg-gray-900 sm:px-3" ]
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
                        , div [ class "flex justify-center" ]
                            [ button
                                [ class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400 w-11/12"
                                , onClick Builder.Opened
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
