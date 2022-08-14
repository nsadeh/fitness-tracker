module Pages.Workouts.WorkoutsPage exposing (Model, Msg(..), dateOf, goToDate, loading, render, update, view, withNavError)

import Actions exposing (Action)
import Array
import Color exposing (Color)
import Date exposing (Date)
import Effects exposing (Effect(..))
import Html exposing (Html, a, button, div, h2, input, p, span, text)
import Html.Attributes as Attributes exposing (class, classList, disabled, height, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Pages.Workouts.ExerciseBuilder as Builder
import Pages.Workouts.ExerciseEditor as Editor
import Pages.Workouts.ExercisePageNavigation as Navigation exposing (NavAction(..))
import Pages.Workouts.Utils exposing (dateToString, smallDateToString)
import Pages.Workouts.WorkoutLogger as Logger exposing (Msg)
import Pages.Workouts.WorkoutsState as State exposing (State)
import StrengthSet exposing (LoggableStrengthExercise, LoggableStrengthSet, asExercise, getSetRanges)
import Svg exposing (svg)
import Svg.Attributes as SvgAttributes
import Swiper
import Utils.Error as Error exposing (RequestError(..))
import Utils.Log exposing (LogLevel(..))
import Utils.OrderedDict as OrderedDict exposing (OrderedDict)
import Utils.OverrideClick exposing (overrideOnClickWith)


type Model
    = Loading Date
    | Loaded State


loading : Date -> Model
loading date =
    Loading date


render : Date -> OrderedDict String LoggableStrengthExercise -> Model
render date workout =
    State.new date workout
        |> Loaded


withState : (State -> State) -> Model -> Model
withState update_ model =
    case model of
        Loading date ->
            Loading date

        Loaded state_ ->
            update_ state_ |> Loaded


updateState : (a -> State) -> ( a, b ) -> ( Model, b )
updateState update_ tup =
    tup
        |> Tuple.mapFirst update_
        |> Tuple.mapFirst Loaded


updateState2 : (a -> State) -> ( a, b, c ) -> ( Model, b, c )
updateState2 update_ ( a, b, c ) =
    let
        ( m, _ ) =
            updateState update_ ( a, b )
    in
    ( m, b, c )


dateOf : Model -> Date
dateOf model =
    case model of
        Loading date ->
            date

        Loaded args ->
            args
                |> State.today



-- Update --


type Msg
    = Edit Editor.Msg
    | Build Builder.Msg
    | LogWorkout Logger.Msg
    | Navigate Navigation.NavAction
    | FetchedWorkout (OrderedDict String LoggableStrengthExercise)
    | FetchedExercise String (Maybe LoggableStrengthExercise)
    | Passthrough


goToDate : Date -> Msg
goToDate date =
    date
        |> Navigation.SelectDate
        |> Navigate


withNavError : String -> Msg
withNavError errMsg =
    errMsg
        |> Navigation.ImproperSelection
        |> Navigate


update : Msg -> Model -> ( Model, List Effect, List Action )
update msg model =
    case model of
        Loading today ->
            case msg of
                FetchedWorkout workout ->
                    render today workout
                        |> Effects.none
                        |> Actions.none

                _ ->
                    model
                        |> Effects.withEffect (Log Error "Unexpected message to loading state")
                        |> Actions.none

        Loaded state ->
            case msg of
                Edit msg_ ->
                    State.editor state
                        |> Editor.update msg_
                        |> updateState (State.updateEditor state)
                        |> Actions.none

                Build msg_ ->
                    State.builder state
                        |> Builder.update msg_
                        |> updateState (State.updateBuilder state)
                        |> Actions.none

                LogWorkout msg_ ->
                    State.logger state
                        |> Logger.update msg_
                        |> updateState (State.updateLogger state)
                        |> Actions.none

                Navigate msg_ ->
                    state
                        |> Navigation.navigatePage msg_
                        |> updateState2 identity

                FetchedWorkout workout ->
                    model
                        |> withState (State.updateWorkout workout)
                        |> Effects.none
                        |> Actions.none

                FetchedExercise exerciseId (Just exercise) ->
                    model
                        |> withState (State.updateExercise exerciseId exercise)
                        |> Effects.none
                        |> Actions.none

                FetchedExercise exerciseId Nothing ->
                    model
                        |> withState (State.deleteExercise exerciseId)
                        |> Effects.none
                        |> Actions.none

                Passthrough ->
                    model
                        |> Effects.none
                        |> Actions.none


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            div [ class "min-h-screen justify-center w-full bg-gray-900 py-50" ]
                [ h2 [ class "text-lg" ]
                    [ text "Loading placeholder"
                    ]
                ]

        Loaded state ->
            div [ class "flex min-h-screen justify-center w-full bg-gray-900 sm:px-3 " ]
                [ state
                    |> State.editor
                    |> Editor.view
                    |> Html.map Edit
                , viewWorkout state
                ]


viewWorkout : State -> Html Msg
viewWorkout state =
    div (class "w-screen text-blue-200" :: (Swiper.onSwipeEvents Navigation.SwipedNavbar |> List.map (Attributes.map Navigate)))
        [ div []
            [ div (class "flex flex-row sm:justify-between justify-center mb-6 p-2 pb-2 mt-3" :: (Swiper.onSwipeEvents Navigation.SwipedNavbar |> List.map (Attributes.map Navigate)))
                [ a [ class "hidden sm:block my-auto hover:text-blue-400" ] [ text "< yesterday" ]
                , h2 [ class "text-4xl text-center" ]
                    [ text (dateToString <| State.today state) ]
                , a [ class "hidden sm:block my-auto hover:text-blue-400" ] [ text "tomorrow >" ]
                ]
            , div [ class "overflow-y-scroll sm:mx-0 mx-1 overflow-x-hidden" ]
                (State.workout state
                    |> OrderedDict.map (viewExercise state)
                    |> OrderedDict.values
                )
            ]
        ]


viewExercise : State -> String -> LoggableStrengthExercise -> Html Msg
viewExercise state exerciseId exercise =
    let
        ( weights, reps ) =
            exercise
                |> StrengthSet.sets
                |> Array.map StrengthSet.todo
                |> Array.toList
                |> getSetRanges
    in
    div []
        [ div
            [ class "whitespace-nowrap overflow-x-scroll snap-x snap-mandatory h-22 exercise-container"
            , onClick (Navigate <| Navigation.expand exerciseId)
            ]
            [ div [ class "w-screen inline-block h-20 snap-start align-middle" ]
                [ div
                    [ classList
                        [ ( "mr-2 ml-1 h-full border-2 border-blue-400 rounded-t-md overflow-scroll", True )
                        , ( "rounded-b-md", not <| State.isToggled exerciseId state )
                        ]
                    ]
                    [ div [ class "flex my-auto whitespace-normal mx-3 mt-2" ]
                        [ div [ class "text-xl font-bold text-blue-300 content-center" ]
                            [ text <| StrengthSet.name exercise
                            ]
                        ]
                    , div [ class "flex flex-row justify-between my-auto mx-3" ]
                        [ div []
                            [ span [ class "text-2xl" ]
                                [ text (String.fromInt <| Array.length <| StrengthSet.sets exercise)
                                ]
                            , span [ class "text-xs" ]
                                [ text "sets"
                                ]
                            ]
                        , div []
                            [ span [ class "text-2xl" ]
                                [ text weights ]
                            , span [ class "text-xs" ]
                                [ text "lbs"
                                ]
                            ]
                        , div []
                            [ span [ class "text-2xl" ]
                                [ text reps ]
                            , span [ class "text-xs" ]
                                [ text "reps"
                                ]
                            ]
                        , div [ class "sm:block hidden px-2" ]
                            [ div [ class "flex flex-row" ]
                                [ button
                                    [ type_ "button"
                                    , class "border-2 border-red-400 rounded-md p-2 hover:bg-red-400"
                                    , overrideOnClickWith (Editor.open exerciseId (asExercise exercise) (State.today state)) |> Attributes.map Edit
                                    ]
                                    [ text "Edit"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , editButton (Edit <| Editor.open exerciseId (asExercise exercise) (State.today state))
            ]
        , div
            [ classList
                [ ( "w-screen transition-[max-height] duration-300", True )
                , ( "h-fit max-h-72 ease-out", State.isToggled exerciseId state )
                , ( "max-h-0 overflow-hidden ease-out", not (State.isToggled exerciseId state) )
                ]
            ]
            [ div [ class "max-h-72 border-b-2 border-x-2 rounded-b-md border-blue-400 mr-2 ml-1 overflow-scroll" ]
                (Array.indexedMap (viewSet state exerciseId) (StrengthSet.sets exercise)
                    |> Array.toList
                )
            ]
        , div [ class "h-3" ] []
        ]


viewSet : State -> String -> Int -> LoggableStrengthSet -> Html Msg
viewSet state exerciseId setNumber set =
    div [ class "flex flex-row border-b border-blue-400 justify-between py-auto px-1 sm:px-2 py-1 h-20" ]
        [ div [ class "flex justify-center my-auto mr-3 sm:block hidden" ]
            [ h2 [ class "text-xl" ]
                [ text (String.fromInt (setNumber + 1) ++ ".")
                ]
            ]
        , div [ class "flex flex-row justify-around my-auto w-full" ]
            [ div [ class "flex flex-row justify-between sm:mr-10 mr-5" ]
                [ div [ class "flex flex-col justify-center" ]
                    [ div [ class "hidden text-lg pb-1" ]
                        [ text
                            (State.logger state
                                |> Logger.loggedSet exerciseId setNumber
                                |> Maybe.map .weight
                                |> Maybe.withDefault (String.fromFloat <| .weight <| StrengthSet.todo set)
                            )
                        , span [ class "text-xs" ] [ text "lbs" ]
                        ]
                    ]
                , div [ class "ml-3 my-auto flex flex-col mr-3" ]
                    [ div [ class "text-xs align-top" ] [ text "today(lbs):" ]
                    , input
                        [ type_ "string"
                        , class "align-middle w-16 h-18 border rounded-md text-black"
                        , value
                            (State.logger state
                                |> Logger.loggedSet exerciseId setNumber
                                |> Maybe.map .weight
                                |> Maybe.withDefault (String.fromFloat <| .weight <| StrengthSet.todo set)
                            )
                        , disabled
                            (State.logger state
                                |> Logger.isLogged exerciseId setNumber
                            )
                        , onInput (Logger.WeightRecorded exerciseId setNumber) |> Attributes.map LogWorkout
                        ]
                        []
                    ]
                , viewLastLoggedWeight set
                ]
            , div [ class "flex flex-row justify-between mr-1" ]
                [ div [ class "my-auto flex flex-col mr-3" ]
                    [ div [ class "text-xs align-top" ] [ text "today(rps):" ]
                    , div [ class "flex flex-row jutify-between" ]
                        [ input
                            [ type_ "string"
                            , class "align-middle w-16 h-18 border rounded-md text-black"
                            , value
                                (State.logger state
                                    |> Logger.loggedSet exerciseId setNumber
                                    |> Maybe.map .reps
                                    |> Maybe.withDefault (String.fromInt <| .reps <| StrengthSet.todo set)
                                )
                            , disabled
                                (State.logger state
                                    |> Logger.isLogged exerciseId setNumber
                                )
                            , onInput (Logger.RepsRecorded exerciseId setNumber) |> Attributes.map LogWorkout
                            ]
                            []
                        ]
                    ]
                , viewLastLoggedReps set
                , if
                    State.logger state
                        |> Logger.isLogged exerciseId setNumber
                  then
                    unlogSetButton

                  else
                    logSetButton
                ]
            ]
        ]


viewLastLoggedWeight : LoggableStrengthSet -> Html msg
viewLastLoggedWeight set_ =
    case StrengthSet.lastLog set_ of
        Nothing ->
            div [ class "my-auto" ] [ p [ class "py-auto text-center text-xs" ] [ text "No prior log" ] ]

        Just ( date, set ) ->
            div
                [ class "flex flex-col justify-items-center" ]
                [ div [ class "text-xs align-top" ] [ text (smallDateToString date) ]
                , div [ class "flex justify-center" ] [ text (String.fromFloat set.weight) ]
                ]


viewLastLoggedReps : LoggableStrengthSet -> Html msg
viewLastLoggedReps set_ =
    case StrengthSet.lastLog set_ of
        Nothing ->
            div [ class "my-auto" ] [ p [ class "py-auto text-center text-xs" ] [ text "No prior log" ] ]

        Just ( date, set ) ->
            div
                [ class "flex flex-col justify-center" ]
                [ div [ class "text-xs align-top" ] [ text (smallDateToString date) ]
                , div [ class "flex justify-center" ] [ text (String.fromInt set.reps) ]
                ]


editButton : Msg -> Html Msg
editButton openEditor =
    div [ class "w-20 snap-start inline-block sm:hidden h-20 align-middle", onClick openEditor ]
        [ div [ class "bg-red-700 text-gray-200 rounded-md h-full block" ]
            [ div [ class "p-3.5 h-fit" ]
                [ div [ class "flex flex-row justify-center" ]
                    [ svg [ height 30, width 30, SvgAttributes.class "fill-gray-400" ]
                        [ Outlined.edit_note 30 Inherit ]
                    ]
                , div [ class "flex flex-row justify-center text-sm" ] [ text "Edit" ]
                ]
            ]
        ]


logSetButton : Html msg
logSetButton =
    div [ class "my-auto fill-green-400 pl-4" ]
        [ svg [ height 30, width 30 ]
            [ Outlined.check_circle_outline 30 (Color <| Color.rgb255 74 222 128) ]
        ]


unlogSetButton : Html msg
unlogSetButton =
    div [ class "my-auto fill-green-400 pl-4" ]
        [ svg [ height 30, width 30, SvgAttributes.class "fill-green-400" ]
            [ Outlined.edit_note 30 (Color <| Color.rgb255 56 189 248) ]
        ]
