module Pages.Workouts.ExerciseView exposing (viewExercise)

import Array
import Color exposing (Color)
import Date exposing (Date)
import Html exposing (Html, button, div, h2, input, span, text)
import Html.Attributes exposing (checked, class, disabled, height, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Pages.Workouts.Utils exposing (smallDateToString)
import Pages.Workouts.WorkoutLogger exposing (Msg(..))
import Pages.Workouts.WorkoutsState exposing (swapState)
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise, StrengthSet, getSetRanges, numSets)
import Svg exposing (svg)
import Svg.Attributes as SvgAttributes
import Swiper
import Utils.Log exposing (LogType(..))
import Utils.OverrideClick exposing (overrideOnClickWith)


viewExercise :
    { expanded : String -> Bool
    , isLoggedToday : String -> Bool
    , getEnteredData : String -> Int -> Maybe { weight : String, reps : String }
    , isEditorOpen : String -> Bool
    , swapState : Bool
    , isChecked : String -> Bool
    }
    ->
        { onOpenWorkoutEditor : String -> msg
        , onToggle : String -> msg
        , onWeightInput : String -> Int -> String -> msg
        , onRepsInput : String -> Int -> String -> msg
        , onLogAction : String -> msg
        , onRelogAction : String -> msg
        , onSwipeExercise : String -> Swiper.SwipeEvent -> msg
        , addToSwap : String -> msg
        , removeFromSwap : String -> msg
        , swapExercises : String -> String -> msg
        }
    -> String
    -> LoggedStrengthExercise
    -> Html msg
viewExercise { expanded, isLoggedToday, getEnteredData, isEditorOpen, swapState, isChecked } { onOpenWorkoutEditor, onToggle, onWeightInput, onRepsInput, onLogAction, onRelogAction, onSwipeExercise, addToSwap, removeFromSwap } id exercise =
    let
        ( weights, reps ) =
            exercise.sets
                |> (\s ->
                        case s of
                            Unlogged { todo } ->
                                todo

                            Logged { sets } ->
                                Array.map (\set -> set.todo) sets
                   )
                |> Array.toList
                |> getSetRanges
    in
    div [ class "exercise-panel mb-3 drop-shadow-2xl flex flex-row overflow-x-scroll snap-x" ]
        [ if (expanded id) then div [] [] else editButton onOpenWorkoutEditor id
        , div
            [ class
                ("snap-start cursor-pointer justify-between "
                    ++ (if expanded id then
                            " border-b border-blue-400"

                        else
                            ""
                       )
                )
            ]
            [ if swapState then
                swapButton isChecked addToSwap removeFromSwap id

              else if isEditorOpen id then
                editButton onOpenWorkoutEditor id

              else
                div [] []
            , div
                [ class "cursor-pointer flex flex-row w-full justify-between p-2  h-20 border rounded-md border-blue-400"
                , onClick (onToggle id)
                ]
                [ div [ class "flex my-auto" ]
                    [ div [ class "w-56 text-xl content-center" ]
                        [ text exercise.name
                        ]
                    ]
                , div [ class "flex flex-row w-30 justify-between my-auto" ]
                    [ div []
                        [ span [ class "text-2xl" ]
                            [ text (String.fromInt (numSets exercise.sets))
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
                    ]
                , div []
                    [ div [ class "flex flex-row" ]
                        [ button
                            [ type_ "button"
                            , class "border-2 border-red-400 w-24 rounded-md m-2 p-2 hover:bg-red-400 sm:block hidden"
                            , overrideOnClickWith (onOpenWorkoutEditor id)
                            ]
                            [ text "Edit"
                            ]

                        -- , button [ type_ "button", class "border-2 border-blue-400 w-24 rounded-md m-2 p-2 hover:bg-blue-400 sm:block hidden", overrideOnClickWith (LogSets id exercise.sets |> Log) ]
                        --     [ text "Log all!"
                        --     ]
                        ]
                    ]
                ]
            ]
        , input
            [ type_ "checkbox"
            , class "opacity-0 h-0 absolute"
            , checked (expanded id)
            , onCheck (\_ -> onToggle id)
            ]
            []
        , div
            [ class
                (if expanded id then
                    "transition-all ease-in-out duration-700 clear-both"

                 else
                    "hidden overflow-hidden transition-all ease-in-out duration-700 clear-both"
                )
            ]
            (List.append
                (let
                    ( date, workoutSets ) =
                        case exercise.sets of
                            Unlogged { todo } ->
                                ( Nothing, Array.map (\set -> ( set, Nothing )) todo )

                            Logged { loggedOn, sets } ->
                                ( Just loggedOn, Array.map (\set -> ( set.todo, Just set.logged )) sets )
                 in
                 Array.indexedMap
                    (viewSet
                        { isLoggedToday = isLoggedToday id
                        , enteredData = getEnteredData id
                        }
                        { onRepsInput = onRepsInput id, onWeightInput = onWeightInput id }
                        date
                    )
                    workoutSets
                    |> Array.toList
                )
                [ div [ class "flex flex-row border-b border-blue-400 justify-center py-auto px-1 sm:px-2 py-1 h-20" ]
                    [ button
                        [ type_ "button"
                        , class
                            ("border-2 border-blue-400 w-32 rounded-md my-auto p-1 hover:bg-blue-400 h-12"
                                ++ (if isLoggedToday id then
                                        " hidden"

                                    else
                                        ""
                                   )
                            )
                        , overrideOnClickWith (onLogAction id)
                        ]
                        [ text "Log Exercise"
                        ]
                    , button
                        [ type_ "button"
                        , class
                            ("border-2 border-red-400 w-32 rounded-md my-auto p-1 hover:bg-red-400 h-12"
                                ++ (if isLoggedToday id then
                                        ""

                                    else
                                        " hidden"
                                   )
                            )
                        , overrideOnClickWith (onRelogAction id)
                        ]
                        [ text "Reopen Log"
                        ]
                    ]
                ]
            )
        ]


viewSet :
    { isLoggedToday : Bool
    , enteredData : Int -> Maybe { reps : String, weight : String }
    }
    ->
        { onWeightInput : Int -> String -> msg
        , onRepsInput : Int -> String -> msg
        }
    -> Maybe Date
    -> Int
    -> ( StrengthSet, Maybe StrengthSet )
    -> Html msg
viewSet { isLoggedToday, enteredData } { onWeightInput, onRepsInput } loggedDate setNumber ( todo, logged ) =
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
                            (enteredData setNumber
                                |> Maybe.map (\d -> d.weight)
                                |> Maybe.withDefault (String.fromFloat todo.weight)
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
                            (enteredData setNumber
                                |> Maybe.map (\d -> d.weight)
                                |> Maybe.withDefault (String.fromFloat todo.weight)
                            )
                        , disabled isLoggedToday
                        , onInput (onWeightInput setNumber)
                        ]
                        []
                    ]
                , viewLastLoggedWeight loggedDate logged
                ]
            , div [ class "flex flex-row justify-between mr-1" ]
                [ div [ class "my-auto flex flex-col mr-3" ]
                    [ div [ class "text-xs align-top" ] [ text "today(rps):" ]
                    , div [ class "flex flex-row jutify-between" ]
                        [ input
                            [ type_ "string"
                            , class "align-middle w-16 h-18 border rounded-md text-black"
                            , value
                                (enteredData setNumber
                                    |> Maybe.map (\d -> d.reps)
                                    |> Maybe.withDefault (String.fromInt todo.reps)
                                )
                            , disabled isLoggedToday
                            , onInput (onRepsInput setNumber)
                            ]
                            []
                        ]
                    ]
                , viewLastLoggedReps loggedDate logged
                , if isLoggedToday then
                    unlogSetButton

                  else
                    logSetButton
                ]
            ]
        ]


viewLastLoggedWeight : Maybe Date -> Maybe StrengthSet -> Html msg
viewLastLoggedWeight maybeDate maybeSet =
    case ( maybeSet, maybeDate ) of
        ( Just set, Just date ) ->
            div
                [ class "flex flex-col justify-items-center" ]
                [ div [ class "text-xs align-top" ] [ text (smallDateToString date) ]
                , div [ class "flex justify-center" ] [ text (String.fromFloat set.weight) ]
                ]

        _ ->
            div [] [ span [ class "py-auto align-middle" ] [ text "No prior log" ] ]


viewLastLoggedReps : Maybe Date -> Maybe StrengthSet -> Html msg
viewLastLoggedReps maybeDate maybeSet =
    case ( maybeSet, maybeDate ) of
        ( Just set, Just date ) ->
            div
                [ class "flex flex-col justify-center" ]
                [ div [ class "text-xs align-top" ] [ text (smallDateToString date) ]
                , div [ class "flex justify-center" ] [ text (String.fromInt set.reps) ]
                ]

        _ ->
            div [] [ text "No prior log" ]


editButton : (String -> msg) -> String -> Html msg
editButton onOpenWorkoutEditor id =
    button [ class "flex flex-col jutify-center h-max bg-red-700 rounded-md px-5 mr-2 snap-end", onClick (onOpenWorkoutEditor id) ]
        [ div [ class "my-auto" ]
            [ svg [ height 30, width 30, SvgAttributes.class "fill-gray-400" ]
                [ Outlined.edit_note 30 Inherit ]
            , div
                [ class "flex justify-center"
                , onClick (onOpenWorkoutEditor id)
                ]
                [ span [ class "text-gray-200 text-sm" ] [ text "Edit" ]
                ]
            ]
        ]


swapButton : (String -> Bool) -> (String -> msg) -> (String -> msg) -> String -> Html msg
swapButton isChecked addToSwap removeFromSwap id =
    input
        [ type_ "checkbox"
        , class "flex justify-center h-max  bg-blue-500 px-4 ml-3"
        , onCheck
            (\b ->
                if b then
                    addToSwap id

                else
                    removeFromSwap id
            )
        , checked (isChecked id)
        ]
        []


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
