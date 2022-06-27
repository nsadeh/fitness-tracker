module Pages.Workouts.ExerciseView exposing (viewExercise)

import Array
import Date exposing (Date)
import Html exposing (Html, button, div, h2, input, span, text)
import Html.Attributes exposing (checked, class, disabled, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Pages.Workouts.Utils exposing (dateToString, getSetRanges)
import Pages.Workouts.WorkoutLogger exposing (Msg(..))
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise, StrengthSet, numSets)
import Utils.OverrideClick exposing (overrideOnClickWith)


viewExercise :
    { expanded : String -> Bool
    , isLogged : String -> Bool
    }
    ->
        { onOpenWorkoutEditor : String -> msg
        , onToggle : String -> msg
        , onWeightInput : String -> Int -> String -> msg
        , onRepsInput : String -> Int -> String -> msg

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
                        { isLogged = isLogged id }
                        { onRepsInput = onRepsInput id, onWeightInput = onWeightInput id }
                        date
                    )
                    workoutSets
                    |> Array.toList
                )
                [ div [ class "flex flex-row border-b border-blue-400 justify-center py-auto px-1 sm:px-2 py-1" ]
                    [ button
                        [ type_ "button"
                        , class "border-2 border-blue-400 w-32 rounded-md m-1 p-1 hover:bg-red-400"
                        ]
                        [ text "Log Exercise"
                        ]
                    ]
                ]
            )
        ]


viewSet :
    { isLogged : Bool }
    ->
        { onWeightInput : Int -> String -> msg
        , onRepsInput : Int -> String -> msg
        }
    -> Maybe Date
    -> Int
    -> ( StrengthSet, Maybe StrengthSet )
    -> Html msg
viewSet { isLogged } { onWeightInput, onRepsInput } loggedDate setNumber ( todo, logged ) =
    div [ class "flex flex-row border-b border-blue-400 justify-between py-auto px-1 sm:px-2 py-1" ]
        [ div [ class "d-flex justify-center my-auto mr-3 sm:block hidden" ]
            [ h2 [ class "text-xl" ]
                [ text (String.fromInt (setNumber + 1) ++ ".")
                ]
            ]
        , div [ class "flex flex-row justify-between my-auto w-full" ]
            [ div [ class "flex flex-row justify-between sm:mr-10 mr-5" ]
                [ viewLastLoggedWeight loggedDate logged
                , div [ class "flex flex-col justify-center" ]
                    [ div [ class "hidden text-lg pb-1" ]
                        [ text (String.fromFloat todo.weight)
                        , span [ class "text-xs" ] [ text "lbs" ]
                        ]
                    ]
                , div [ class "ml-3 my-auto flex flex-col" ]
                    [ div [ class "text-xs align-top" ] [ text "today(lbs):" ]
                    , input
                        [ type_ "number"
                        , class "align-middle w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromFloat todo.weight)
                        , disabled isLogged
                        , onInput (onWeightInput setNumber)
                        ]
                        []
                    ]
                ]
            , div [ class "flex flex-row justify-between mr-1" ]
                [ viewLastLoggedSet loggedDate logged
                , div [ class "ml-3 my-auto flex flex-col" ]
                    [ div [ class "text-xs align-top" ] [ text "today(rps):" ]
                    , input
                        [ type_ "number"
                        , class "align-middle w-16 border rounded-md bg-blue-100 text-black"
                        , value (String.fromInt todo.reps)
                        , disabled isLogged
                        , onInput (onRepsInput setNumber)
                        ]
                        []
                    ]
                ]
            ]

        -- , div []
        --     [ button
        --         [ type_ "button"
        --         , class "border-2 border-blue-400 sm:w-24 w-20 rounded-md sm:m-2 my-2 ml-1 sm:p-2 p-1 hover:bg-blue-400"
        --         -- , overrideOnClickWith onLogAction
        --         ]
        --         [ text "Record"]
        --     ]
        ]


viewLastLoggedWeight : Maybe Date -> Maybe StrengthSet -> Html msg
viewLastLoggedWeight maybeDate maybeSet =
    case ( maybeSet, maybeDate ) of
        ( Just set, Just date ) ->
            div
                [ class "flex flex-col justify-center" ]
                [ div [ class "text-xs align-top" ] [ text (String.append " (lbs)" <| dateToString date) ]
                , div [] [ text (String.fromFloat set.weight) ]
                ]

        _ ->
            div [] [ span [ class "py-auto align-middle"] [ text "No prior log" ] ] 


viewLastLoggedSet : Maybe Date -> Maybe StrengthSet -> Html msg
viewLastLoggedSet maybeDate maybeSet =
    case ( maybeSet, maybeDate ) of
        ( Just set, Just date ) ->
            div
                [ class "flex flex-col justify-center" ]
                [ div [ class "text-xs align-top" ] [ text (String.append " (reps)" <| dateToString date) ]
                , div [] [ text (String.fromFloat set.weight) ]
                ]

        _ ->
            div [] [ text "No prior log" ]
