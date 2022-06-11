module Pages.Workouts.ExerciseView exposing (..)

import Html exposing (Html, button, div, h2, input, span, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onInput)
import StrengthSet exposing (LoggedStrenghtSet)
import Utils.OverrideClick exposing (overrideOnClickWith)


viewSet : { isLogged : Bool } -> { onWeightInput : String -> msg, onRepsInput : String -> msg, onLogAction : msg } -> Int -> LoggedStrenghtSet -> Html msg
viewSet { isLogged } { onWeightInput, onRepsInput, onLogAction } setNumber set =
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
                        [ text (String.fromFloat set.todo.weight)
                        , span [ class "text-xs" ] [ text "lbs" ]
                        ]

                    -- , viewLastWeek True set.logged
                    ]
                , div [ class "ml-3 my-auto flex flex-col" ]
                    [ div [ class "text-xs align-top" ] [ text "today(lbs):" ]
                    , input
                        [ type_ "number"
                        , class "align-middle w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromFloat set.todo.weight)
                        , disabled isLogged
                        , onInput onWeightInput
                        ]
                        []
                    ]
                ]
            , div [ class "flex flex-row justify-between mr-1" ]
                [ div [ class "flex flex-col justify-center" ]
                    [ div [ class "hidden text-xl pb-1" ]
                        [ text (String.fromInt set.todo.reps)
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
                        , value (String.fromInt set.todo.reps)
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
                , overrideOnClickWith onLogAction
                ] []
            ]
        ]

