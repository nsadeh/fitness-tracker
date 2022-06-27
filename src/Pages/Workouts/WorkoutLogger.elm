module Pages.Workouts.WorkoutLogger exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Dict exposing (Dict)
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise, StrengthSet)
import Utils.OrderedDict exposing (OrderedDict)
import Api.Exercises as Exercise
import Utils.Log exposing (LogType(..))

type alias Model =
    Dict String { isLogged : Bool, draft : Array { reps : String, weight : String } }


type Values
    = Reps
    | Weight


type Msg
    = RepsRecorded String Int String
    | WeightRecorded String Int String
    | RequestLogWorkout String
    | LoggedWorkout String
    | FailedToLog


init : Date -> OrderedDict String LoggedStrengthExercise -> Model
init today workout =
    Utils.OrderedDict.toDict workout
        |> Dict.map
            (\_ exercise ->
                asLoggable today exercise
            )


update : { log : String -> List StrengthSet -> Cmd msg } -> Msg -> Model -> ( Model, Cmd msg )
update { log } msg model =
    case msg of
        RepsRecorded id setNumber reps ->
            ( Dict.update id (Maybe.map (\m -> { m | draft = updateValue Reps setNumber reps m.draft })) model, Cmd.none )

        WeightRecorded id setNumber weight ->
            ( Dict.update id (Maybe.map (\m -> { m | draft = updateValue Weight setNumber weight m.draft })) model, Cmd.none )

        LoggedWorkout id ->
            ( Dict.update id (Maybe.map (\draft -> { draft | isLogged = True })) model, Cmd.none )

        RequestLogWorkout id ->
            ( model
            , toExercise id model
                |> Maybe.map Array.toList
                |> Maybe.map (log id)
                |> Maybe.withDefault Cmd.none
            )

        FailedToLog ->
            Utils.Log.log Error "This is not implemented" model


updateValue : Values -> Int -> String -> Array { reps : String, weight : String } -> Array { reps : String, weight : String }
updateValue value setNumber quantity array =
    let
        entry =
            Array.get setNumber array

        updated =
            case value of
                Reps ->
                    Maybe.map (\e -> { e | reps = quantity }) entry
                        |> Maybe.withDefault { reps = quantity, weight = "0" }

                Weight ->
                    Maybe.map (\e -> { e | reps = quantity }) entry
                        |> Maybe.withDefault { reps = quantity, weight = "0" }
    in
    Array.set setNumber updated array


toExercise : String -> Model -> Maybe (Array StrengthSet)
toExercise id model =
    Dict.get id model
        |> Maybe.andThen
            (\value ->
                List.map asSet (Array.toList value.draft)
                    |> List.foldr (Maybe.map2 (::)) (Just [])
                    |> Maybe.map Array.fromList
            )


asSet : { reps : String, weight : String } -> Maybe StrengthSet
asSet { reps, weight } =
    Maybe.map2 StrengthSet (String.toInt reps) (String.toFloat weight)


asLoggable : Date -> LoggedStrengthExercise -> { isLogged : Bool, draft : Array { reps : String, weight : String } }
asLoggable today exercise =
    case exercise.sets of
        Unlogged { todo } ->
            { isLogged = False
            , draft =
                Array.map
                    (\set ->
                        { reps = String.fromInt set.reps, weight = String.fromFloat set.weight }
                    )
                    todo
            }

        Logged { loggedOn, sets } ->
            { isLogged = loggedOn == today
            , draft =
                Array.map
                    (\set ->
                        { reps = String.fromInt set.todo.reps, weight = String.fromFloat set.todo.weight }
                    )
                    sets
            }
