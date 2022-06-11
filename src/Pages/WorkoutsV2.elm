module Pages.WorkoutsV2 exposing (..)

import Api.Exercises as Excercises
import Api.Supabase exposing (AuthenticatedUser)
import Browser.Navigation as Nav exposing (Key)
import Date exposing (Date)
import Html exposing (Html, div, i)
import Html.Attributes exposing (class)
import Pages.Login exposing (Msg)
import Set exposing (Set)
import StrengthSet exposing (LoggedStrengthExercise, StrengthExercise)
import Swiper
import Utils.OrderedDict exposing (OrderedDict)
import WorkoutCreator exposing (WorkoutCreator)



-- model --


type alias WorkoutsPageState =
    { api : Excercises.API
    , user : AuthenticatedUser
    , navKey : Nav.Key
    , today : Date
    , swipeState : Swiper.SwipingState
    , editor : Maybe ( String, StrengthExercise )
    , creator : WorkoutCreator
    , open : Set String
    , workout : OrderedDict String LoggedStrengthExercise
    }


type Model
    = Unathenticated
    | Authenticated WorkoutsPageState



-- view --
-- view : Model -> Html Msg
-- view model =
--     case model of
--         Unathenticated ->
--             div [ class "text-5xl"] [ text "You must login to view this page"]
--         Authenticated { api, user, navKey, today, swipeState, editor, creator, open, workout } ->
--           let
--             exericses = OrderedDict.m
--           in


viewExercise : WorkoutsPageState -> (String -> LoggedStrengthExercise -> Html Msg)
viewExercise state =
    \exerciseID exercise ->
        div [ class "border rounded-md border-blue-400 mb-3 drop-shadow-2xl h-fit" ]
            [ div
                [ class
                    ("cursor-pointer flex flex-row justify-between py-2 px-2"
                        ++ (if (Set.member exerciseID state.open) then
                                " border-b border-blue-400"

                            else
                                ""
                           )
                    )
                , onClick (Toggled id |> Select)
                ]
                [ div [ class "flex my-auto" ]
                    [ div [ class "w-56 text-xl content-center" ]
                        [ text exercise.name
                        ]
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
                (List.indexedMap (viewSet logged id) workoutSets)
            ]
