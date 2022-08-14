module Pages.Workouts.ExerciseBuilder exposing (Model, Msg(..), closed, open, update, view)

import Array exposing (Array)
import Date exposing (Date)
import Dict exposing (Dict)
import Effects as Effects exposing (Effect(..))
import Html exposing (Html, button, div, form, h4, input, text)
import Html.Attributes exposing (class, maxlength, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import StrengthSet exposing (StrengthExercise, StrengthSet, removeSet)
import Time exposing (Month(..))
import Utils.Log exposing (LogLevel(..))


type alias BuilderArgs =
    { name : String, numSets : Int, sets : Dict Int { reps : String, weight : String }, date : Date, order : Int }


type Model
    = Closed
    | Open BuilderArgs


closed : Model
closed =
    Closed


open : Date -> Int -> Model
open date order =
    Open { name = "", numSets = 0, sets = Dict.empty, date = date, order = order }


clear : Model -> Model
clear model =
    updateData (\args -> { args | numSets = 0, name = "", sets = Dict.empty }) model


updateSetNumber : Int -> BuilderArgs -> BuilderArgs
updateSetNumber setNumber args =
    { args | numSets = setNumber }


updateData : (BuilderArgs -> BuilderArgs) -> Model -> Model
updateData updateF model =
    case model of
        Closed ->
            Closed

        Open args ->
            Open <| updateF args


isValidSet : { reps : String, weight : String } -> Maybe StrengthSet
isValidSet { reps, weight } =
    Maybe.map2 (\r w -> { reps = r, weight = w }) (String.toInt reps) (String.toFloat weight)


reducer : Maybe StrengthSet -> Maybe (Array StrengthSet) -> Maybe (Array StrengthSet)
reducer set arr =
    Maybe.andThen (\s -> Maybe.map (\a -> Array.push s a) arr) set


parse : Dict Int { reps : String, weight : String } -> Maybe (Array StrengthSet)
parse entries =
    Dict.values entries
        |> List.map isValidSet
        |> List.foldl reducer (Just Array.empty)


submit : BuilderArgs -> Msg
submit args =
    Maybe.map (\sets -> { name = args.name, sets = sets }) (parse args.sets)
        |> Maybe.map (Submitted args.date args.order)
        |> Maybe.withDefault Invalid


addSet : Model -> Model
addSet model =
    updateData (\args -> { args | numSets = args.numSets + 1 }) model


removeSet : Int -> Model -> Model
removeSet setNumber model =
    updateData (\args -> { args | numSets = args.numSets - 1, sets = Dict.remove setNumber args.sets }) model


updateName : String -> Model -> Model
updateName newName model =
    updateData (\args -> { args | name = newName }) model


updateWeight : Int -> String -> Model -> Model
updateWeight setNumber weightString model =
    updateData
        (\args ->
            case Dict.get setNumber args.sets of
                Nothing ->
                    { args | sets = Dict.insert setNumber { reps = "0", weight = weightString } args.sets }

                Just set ->
                    { args | sets = Dict.insert setNumber { reps = set.reps, weight = weightString } args.sets }
        )
        model


updateReps : Int -> String -> Model -> Model
updateReps setNumber repsString model =
    updateData
        (\args ->
            case Dict.get setNumber args.sets of
                Nothing ->
                    { args | sets = Dict.insert setNumber { reps = repsString, weight = "0.0" } args.sets }

                Just set ->
                    { args | sets = Dict.insert setNumber { reps = repsString, weight = set.weight } args.sets }
        )
        model



-- Update --


type Msg
    = Opened Date Int
    | Close
    | Cleared
    | SetAdded
    | SetRemoved Int
    | NumSetsEntered Int
    | ChangedName String
    | UpdatedWeight Int String
    | UpdatedReps Int String
    | Submitted Date Int StrengthExercise
    | Invalid


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        Opened date order ->
            open date order
                |> Effects.none

        Close ->
            closed
                |> Effects.none

        Cleared ->
            model
                |> clear
                |> Effects.none

        SetAdded ->
            model
                |> addSet
                |> Effects.none

        SetRemoved setNumber ->
            model
                |> removeSet setNumber
                |> Effects.none

        NumSetsEntered setNumber ->
            updateData (updateSetNumber setNumber) model
                |> Effects.none

        ChangedName newName ->
            model
                |> updateName newName
                |> Effects.none

        UpdatedWeight setNumber weight ->
            model
                |> updateWeight setNumber weight
                |> Effects.none

        UpdatedReps setNumber reps ->
            model
                |> updateReps setNumber reps
                |> Effects.none

        Submitted date order exercise ->
            model
                |> Effects.withEffect (CreateExercise date exercise order)

        Invalid ->
            model
                |> Effects.withEffect (Log Error "Invalid exercise builder action")


view : Model -> Html Msg
view model =
    case model of
        Closed ->
            div [] []

        Open args ->
            viewArgs args


viewArgs : BuilderArgs -> Html Msg
viewArgs builder =
    div []
        [ div [ class "flex flex-col mx-1 h-fit border border-blue-400 rounded-md max-h-56" ]
            [ div [ class "flex sm:flex-row flex-col sm:justify-around justify-center sm:h-16 h-fit sm:pl-0 w-full" ]
                [ div [ class "flex flex-row justify-center my-auto" ]
                    [ input
                        [ class "border rounded-md bg-blue-100 text-black h-12 w-10/12 px-2 my-4"
                        , placeholder "Name"
                        , onInput ChangedName
                        , value builder.name
                        , maxlength 24
                        ]
                        []
                    ]
                , div [ class "flex flex-row my-auto justify-center" ]
                    [ input
                        [ class "w-10/12 border rounded-md bg-blue-100 text-black h-12 px-2 mb-4"
                        , placeholder "# Sets"
                        , type_ "number"
                        , value
                            (if builder.numSets == 0 then
                                ""

                             else
                                String.fromInt builder.numSets
                            )
                        , onInput (\s -> NumSetsEntered <| Maybe.withDefault 0 <| String.toInt s)
                        ]
                        []
                    ]
                ]
            , div [] [ viewSetForm builder ]
            , div [ class "flex justify-center" ]
                [ button [ class "border-2 border-blue-400 w-30 rounded-md mt-1 mb-3 p-2 hover:bg-blue-400", onClick (submit builder) ]
                    [ text "Create set!" ]
                ]
            ]
        ]


viewSetForm : BuilderArgs -> Html Msg
viewSetForm form =
    div [ class "flex flex-col overflow-y-scroll overflow-x-hidden" ] (List.range 1 form.numSets |> List.map viewFormSingleSet)


viewFormSingleSet : Int -> Html Msg
viewFormSingleSet index =
    div [ class "flex flex-row justify-center" ]
        [ div [ class "flex flex-row justify-between pb-3 w-10/12" ]
            [ div [ class "w-6 my-auto" ]
                [ h4 [ class "text-2xl" ] [ text <| String.fromInt index ]
                ]
            , div [ class "flex flow-row" ]
                [ input [ class "sm:w-fit w-32 h-10 flex border rounded-md bg-blue-100 text-black pl-1 pr-3", placeholder "Starting weight", type_ "number", onInput (UpdatedWeight index) ] []
                ]
            , div [ class "flex flow-row" ]
                [ input [ class "sm:w-fit w-32 h-10 flex border rounded-md bg-blue-100 text-black pl-1 pr-3", placeholder "Starting reps", type_ "number", onInput (UpdatedReps index) ] []
                ]
            ]
        ]
