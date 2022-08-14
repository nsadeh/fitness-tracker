module Pages.Workouts.WorkoutLogger exposing (Model, Msg(..), isLogged, loggedSet, new, update)

import Array exposing (Array, set)
import Date exposing (Date)
import Dict exposing (Dict)
import Effects as Effects exposing (Effect(..), withEffect)
import StrengthSet exposing (LoggableStrengthExercise, LoggableStrengthSet(..), StrengthSet)
import Utils.Log exposing (LogLevel(..))
import Utils.OrderedDict exposing (OrderedDict)


type alias LoggingArgs =
    { date : Date
    , log : Dict String (Array { reps : String, weight : String, logged : Bool })
    }


type Model
    = Logger LoggingArgs


type Values
    = Reps
    | Weight


new : Date -> OrderedDict String LoggableStrengthExercise -> Model
new date_ workout =
    Utils.OrderedDict.toDict workout
        |> Dict.map (\_ exercise -> asLoggable date_ exercise)
        |> (\log -> Logger { date = date_, log = log })


date : Model -> Date
date (Logger args) =
    args.date


updateValue : Values -> Int -> String -> Array { reps : String, weight : String, logged : Bool } -> Array { reps : String, weight : String, logged : Bool }
updateValue value setNumber quantity array =
    let
        entry =
            Array.get setNumber array

        updated =
            case value of
                Reps ->
                    Maybe.map (\e -> { e | reps = quantity }) entry
                        |> Maybe.withDefault { reps = quantity, weight = "0", logged = False }

                Weight ->
                    Maybe.map (\e -> { e | weight = quantity }) entry
                        |> Maybe.withDefault { reps = quantity, weight = "0", logged = False }
    in
    Array.set setNumber updated array


updateLog : Values -> String -> Int -> String -> Model -> Model
updateLog logType exerciseId setNumber logString (Logger args) =
    args.log
        |> Dict.get exerciseId
        |> Maybe.map (updateValue logType setNumber logString)
        |> Maybe.map (\log -> Logger { args | log = Dict.insert exerciseId log args.log })
        |> Maybe.withDefault (Logger args)


markLogged : String -> Int -> Bool -> Model -> Model
markLogged exerciseId setNumber logged (Logger args) =
    args.log
        |> Dict.update exerciseId (Maybe.map (markLoggedLog setNumber logged))
        |> (\d ->
                { args | log = d }
                    |> Logger
           )


markLoggedLog : Int -> Bool -> Array { a | logged : Bool } -> Array { a | logged : Bool }
markLoggedLog setNumber logged current =
    current
        |> Array.get setNumber
        |> Maybe.map (\x -> { x | logged = logged })
        |> Maybe.map (\x -> Array.set setNumber x current)
        |> Maybe.withDefault current


type Msg
    = RepsRecorded String Int String
    | WeightRecorded String Int String
    | SubmitLog String
    | LoggedSet String Int
    | FailedToLog
    | CancelLog String Int


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        RepsRecorded exerciseId setNumber repString ->
            model
                |> updateLog Reps exerciseId setNumber repString
                |> Effects.none

        WeightRecorded exerciseId setNumber weightString ->
            model
                |> updateLog Weight exerciseId setNumber weightString
                |> Effects.none

        SubmitLog exerciseId ->
            model
                |> (toExercise exerciseId model
                        |> Maybe.map Array.toList
                        |> Maybe.map (LogWorkout (date model) exerciseId)
                        |> Maybe.map Effects.withEffect
                        |> Maybe.withDefault Effects.none
                   )

        LoggedSet exerciseId setNumber ->
            model
                |> markLogged exerciseId setNumber True
                |> withEffect (FetchExercise (date model) exerciseId)

        FailedToLog ->
            model
                |> withEffect (Log Error "Failed to log!")

        CancelLog exerciseId setNumber ->
            model
                |> markLogged exerciseId setNumber False
                |> Effects.none


toExercise : String -> Model -> Maybe (Array StrengthSet)
toExercise id (Logger args) =
    Dict.get id args.log
        |> Maybe.andThen
            (\log_ ->
                List.map asSet (Array.toList log_)
                    |> List.foldr (Maybe.map2 (::)) (Just [])
                    |> Maybe.map Array.fromList
            )


asSet : { a | reps : String, weight : String } -> Maybe StrengthSet
asSet { reps, weight } =
    Maybe.map2 StrengthSet (String.toInt reps) (String.toFloat weight)


asLoggable : Date -> LoggableStrengthExercise -> Array { reps : String, weight : String, logged : Bool }
asLoggable today exercise =
    Array.map (asLoggableSet today) (StrengthSet.sets exercise)


asLoggableSet : Date -> LoggableStrengthSet -> { reps : String, weight : String, logged : Bool }
asLoggableSet today set =
    case set of
        Unlogged todo ->
            { reps = String.fromInt todo.reps, weight = String.fromFloat todo.weight, logged = False }

        Logged todo date_ _ ->
            { reps = String.fromInt todo.reps, weight = String.fromFloat todo.weight, logged = Date.compare today date_ == EQ }



-- getters --


loggedSet : String -> Int -> Model -> Maybe { reps : String, weight : String, logged : Bool }
loggedSet exerciseId setNumber (Logger args) =
    Dict.get exerciseId args.log
        |> Maybe.andThen (Array.get setNumber)


isLogged : String -> Int -> Model -> Bool
isLogged exerciseId setNumber model =
    loggedSet exerciseId setNumber model
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False



-- Message producers
