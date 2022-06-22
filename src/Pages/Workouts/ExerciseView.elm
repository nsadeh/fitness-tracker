module Pages.Workouts.ExerciseView exposing (viewExercise)

import Array
import Html exposing (Html, button, div, h2, input, span, text)
import Html.Attributes exposing (checked, class, disabled, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Pages.Workouts.Utils exposing (getSetRanges)
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise, StrengthSet, numSets)
import Utils.OverrideClick exposing (overrideOnClickWith)


viewExercise :
    { expanded : String -> Bool
    , isLogged : String -> Bool
    }
    ->
        { onOpenWorkoutEditor : String -> msg
        , onToggle : String -> msg
        , onWeightInput : String -> String -> msg
        , onRepsInput : String -> String -> msg

        -- , onLogAction : msg
        }
    -> String
    -> LoggedStrengthExercise
    -> Html msg
viewExercise { expanded, isLogged } { onOpenWorkoutEditor, onToggle, onWeightInput, onRepsInput } id exercise =
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
    div [ class "border rounded-md border-blue-400 mb-3 drop-shadow-2xl h-fit" ]
        [ div
            [ class
                ("cursor-pointer flex flex-row justify-between py-2 px-2"
                    ++ (if expanded id then
                            " border-b border-blue-400"

                        else
                            ""
                       )
                )
            , onClick (onToggle id)
            ]
            [ div [ class "flex my-auto" ]
                [ div [ class "w-56 text-xl content-center" ]
                    [ text exercise.name
                    ]
                ]
            , div [ class "flex flex-row w-48 justify-between my-auto" ]
                [ div []
                    [ span [ class "text-xl" ]
                        [ text (String.fromInt (numSets exercise.sets))
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
            (Array.indexedMap
                (viewSet
                    { isLogged = isLogged id }
                    { onWeightInput = onWeightInput id
                    , onRepsInput = onRepsInput id
                    }
                )
                (case exercise.sets of
                    Unlogged { todo } ->
                        Array.map (\set -> ( set, Nothing )) todo

                    Logged { sets } ->
                        Array.map (\set -> ( set.todo, Just set.logged )) sets
                )
                |> Array.toList
            )
        ]


viewSet :
    { isLogged : Bool }
    -> { onWeightInput : String -> msg, onRepsInput : String -> msg }
    -> Int
    -> ( StrengthSet, Maybe StrengthSet )
    -> Html msg
viewSet { isLogged } { onWeightInput, onRepsInput } setNumber ( todo, logged ) =
    div [ class "flex flex-row border-b border-blue-400 justify-between py-auto px-1 sm:px-2 py-1" ]
        [ div [ class "d-flex justify-center my-auto mr-3 sm:block hidden" ]
            [ h2 [ class "text-xl" ]
                [ text (String.fromInt (setNumber + 1) ++ ".")
                ]
            ]
        , div [ class "flex flex-row justify-between my-auto w-auto" ]
            [ div [ class "flex flex-row justify-between sm:mr-10 mr-5" ]
                [ div [ class "flex flex-col justify-center" ]
                    [ div [ class "hidden text-lg pb-1" ]
                        [ text (String.fromFloat todo.weight)
                        , span [ class "text-xs" ] [ text "lbs" ]
                        ]

                    -- , viewLastWeek True set.logged
                    ]
                , div [ class "ml-3 my-auto flex flex-col" ]
                    [ div [ class "text-xs align-top" ] [ text "today(lbs):" ]
                    , input
                        [ type_ "number"
                        , class "align-middle w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromFloat todo.weight)
                        , disabled isLogged
                        , onInput onWeightInput
                        ]
                        []
                    ]
                ]
            , div [ class "flex flex-row justify-between mr-1" ]
                [ div [ class "flex flex-col justify-center" ]
                    [ div [ class "hidden text-xl pb-1" ]
                        [ text (String.fromInt todo.reps)
                        , span [ class "text-xs" ]
                            [ text "reps"
                            ]
                        ]

                    -- , viewLastWeek False set.logged
                    ]
                , div [ class "ml-3 my-auto flex flex-col" ]
                    [ div [ class "text-xs align-top" ] [ text "today(rps):" ]
                    , input
                        [ type_ "number"
                        , class "align-middle w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromInt todo.reps)
                        , disabled isLogged
                        , onInput onRepsInput
                        ]
                        []
                    ]
                ]
            ]
        , div []
            [ button
                [ type_ "button"
                , class "border-2 border-blue-400 sm:w-24 w-20 rounded-md sm:m-2 my-2 ml-1 sm:p-2 p-1 hover:bg-blue-400"

                -- , overrideOnClickWith onLogAction
                ]
                []
            ]
        ]
