module Api.Exercises exposing (API, DeleteExerciseRequest, InsertPayload, LogSetRequest, api)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, Url)
import Array
import Date exposing (Date, Weekday, numberToWeekday, weekdayToNumber)
import Http as H
import Json.Decode as D
import Json.Decode.Extra exposing (..)
import Json.Encode as E
import Pages.Workouts.Utils exposing (padLists)
import Platform exposing (Task)
import Result as Result
import StrengthSet exposing (LoggableStrengthSets(..), LoggedStrengthExercise, StrengthExercise, StrengthSet, decodeExercise, decodeSet, encodeExercise, encodeSet)
import Task
import Time
import Utils.Error exposing (RequestError(..), responseResult)
import Utils.OrderedDict exposing (OrderedDict, empty, filter, insert, map, remove, update)
import Workout exposing (Workout)
import StrengthSet exposing (logSetsInExercise)
import Utils.Log exposing (log)
import Utils.Log exposing (LogType(..))


{-| Exercises API

This is an API to interact with the workouts data

-}
type alias ExerciseId =
    String


type alias API =
    { getLoggedWorkouts : Date -> Task RequestError (OrderedDict String LoggedStrengthExercise)
    , insert : InsertPayload -> Task RequestError ()
    , logSet : ExerciseId -> Date -> Int -> StrengthSet -> Task RequestError ()
    , logExercise : ExerciseId -> Date -> List StrengthSet -> Task RequestError ()
    , deleteExercise : ExerciseId -> Date -> Task RequestError ()
    , editSets : ExerciseId -> List StrengthSet -> Task RequestError ()
    , editName : ExerciseId -> String -> Task RequestError ()
    }


api : Url -> ApiKey -> AuthenticatedUser -> API
api url key user =
    { getLoggedWorkouts = getToday foldLoggedRow url key user
    , insert = \p -> insertJournalEntry url key user ( "", Insert p )
    , logSet = \id loggedOn index set -> insertJournalEntry url key user ( id, LogSet { set = set, number = index, loggedOn = loggedOn } )
    , deleteExercise = \id date -> insertJournalEntry url key user ( id, Delete (DeleteExerciseRequest date) )
    , editSets = \id sets -> insertJournalEntry url key user ( id, Edit (ChangeSetsAndReps sets) )
    , editName = \id name -> insertJournalEntry url key user ( id, Edit (ChangeName name) )
    , logExercise = \id date sets -> insertJournalEntry url key user ( id, LogExercise { sets = sets, loggedOn = date } )
    }



-- Implementation --


getToday : (JournalRow -> OrderedDict String ( Int, m ) -> OrderedDict String ( Int, m )) -> AuthenticatedRequest Date (OrderedDict String m)
getToday parser url key user date =
    let
        rows =
            getJournal url key user ()

        day =
            Date.weekday date
    in
    rows
        |> mapTaskResult (List.foldl parser empty)
        |> mapTaskResult (filter (\_ tuple -> Tuple.first tuple == weekdayToNumber day))
        |> mapTaskResult (map (\_ tuple -> Tuple.second tuple))


getJournal : AuthenticatedRequest () (List JournalRow)
getJournal url key user _ =
    H.task
        { method = "GET"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal?user_id=eq." ++ user.userId ++ "&deleted=eq.false"
        , body = H.emptyBody
        , resolver = H.stringResolver resolveRow
        , timeout = Nothing
        }



-- getLoggedExercises : AuthenticatedRequest Date Workout
-- getLoggedExercises url key user date =
--     let
--         rows = getJournal url key user ()
--     in


-- getExercises : AuthenticatedRequest Date Workout
-- getExercises =
--     getToday foldRow



-- let
--     rows = getJournal url key user ()
--     day =
--         Date.weekday date
-- in
-- rows
--     |> mapTaskResult (List.foldl foldRow empty)
--     |> mapTaskResult (filter (\_ tuple -> Tuple.first tuple == weekdayToNumber day))
--     |> mapTaskResult (map (\_ tuple -> Tuple.second tuple))


insertJournalEntry : AuthenticatedRequest ( String, Action ) ()
insertJournalEntry url key user ( id, action ) =
    let
        body =
            case action of
                Insert request ->
                    encodeInsertRequest request

                Delete request ->
                    encodeDeleteRequest id request

                LogSet { loggedOn, set, number } ->
                    encodeLogSet { exerciseId = id, loggedOn = loggedOn, set = set, index = number }

                Edit request ->
                    encodeEditRequest id request

                LogExercise { loggedOn, sets } ->
                    encodeLogExercise id loggedOn sets
    in
    H.task
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal"
        , body = H.jsonBody body
        , resolver = H.stringResolver resolveNothing
        , timeout = Nothing
        }


-- foldRow : JournalRow -> OrderedDict String ( Int, StrengthExercise ) -> OrderedDict String ( Int, StrengthExercise )
-- foldRow row workouts =
--     case row.action of
--         Insert request ->
--             insert row.exerciseId ( weekdayToNumber request.day, request.exercise ) workouts

--         LogSet _ ->
--             workouts

--         Delete _ ->
--             remove row.exerciseId workouts

--         Edit changeRequest ->
--             case changeRequest of
--                 ChangeName name ->
--                     update row.exerciseId (Maybe.map (\( day, exercise ) -> ( day, { exercise | name = name } ))) workouts

--                 ChangeSetsAndReps sets ->
--                     update row.exerciseId (Maybe.map (\( day, exercise ) -> ( day, { exercise | sets = Array.fromList sets } ))) workouts

--         LogExercise { loggedOn, sets } ->
--             log Info "not implemented"


foldLoggedRow : JournalRow -> OrderedDict String ( Int, LoggedStrengthExercise ) -> OrderedDict String ( Int, LoggedStrengthExercise )
foldLoggedRow row workouts =
    case row.action of
        Insert insertRequest ->
            let
                exercise =
                    { name = insertRequest.exercise.name
                    , sets = Unlogged { todo = insertRequest.exercise.sets }
                    , loggedOn = Nothing
                    }
            in
            insert row.exerciseId ( weekdayToNumber insertRequest.day, exercise ) workouts

        -- deprecated --
        LogSet _ ->
            workouts

        Delete _ ->
            remove row.exerciseId workouts

        Edit cr ->
            case cr of
                ChangeName name ->
                    update row.exerciseId (Tuple.mapSecond (\ex -> { ex | name = name }) |> Maybe.map) workouts

                ChangeSetsAndReps newSets ->
                    update row.exerciseId
                        (Maybe.map <|
                            Tuple.mapSecond
                                (\ex ->
                                    { ex
                                        | sets =
                                            case ex.sets of
                                                Logged { loggedOn, sets } ->
                                                    let
                                                        ll =
                                                            Array.map (\s -> s.logged) sets |> Array.toList

                                                        ( todoList, loggedList ) =
                                                            padLists newSets ll StrengthSet.emptySet StrengthSet.emptySet
                                                    in
                                                    Logged
                                                        { loggedOn = loggedOn
                                                        , sets =
                                                            List.map2 (\todo logged -> { todo = todo, logged = logged })
                                                                todoList
                                                                loggedList
                                                                |> Array.fromList
                                                        }

                                                Unlogged _ ->
                                                    Unlogged { todo = newSets |> Array.fromList }
                                    }
                                )
                        )
                        workouts

        LogExercise { loggedOn, sets } ->
            update row.exerciseId (Maybe.map <| Tuple.mapSecond <| logSetsInExercise loggedOn sets) workouts


mapTaskResult : (r -> s) -> Task RequestError r -> Task RequestError s
mapTaskResult mapper task =
    Task.map mapper task



-- Resolvers --


resolveNothing : H.Response String -> Result RequestError ()
resolveNothing response =
    Result.map (\_ -> ()) (responseResult response)


resolveRow : H.Response String -> Result RequestError (List JournalRow)
resolveRow response =
    let
        decodeJournalEntries =
            \list ->
                D.decodeString (D.list decodeRow) list
                    |> Result.mapError Parsing
    in
    responseResult response
        |> Result.andThen decodeJournalEntries



-- Decoders/Encoders --


decodeRow : D.Decoder JournalRow
decodeRow =
    D.andThen (\raw -> rowWithAction <| decodePayload raw.actionType) decodeRaw


decodeRaw : D.Decoder RawJournalRow
decodeRaw =
    D.map6 RawJournalRow
        (D.field "exercise_id" D.string)
        (D.field "sequence_number" D.int)
        (D.field "timestamp" datetime)
        (D.field "deleted" D.bool)
        (D.field "action_type" D.string)
        (D.field "payload" D.value)


rowWithAction : D.Decoder Action -> D.Decoder JournalRow
rowWithAction decoder =
    D.map5 JournalRow
        (D.field "exercise_id" D.string)
        (D.field "sequence_number" D.int)
        (D.field "timestamp" datetime)
        (D.field "deleted" D.bool)
        (D.field "payload" decoder)


decodeDelete : D.Decoder DeleteExerciseRequest
decodeDelete =
    D.map DeleteExerciseRequest
        (D.field "asOfDate" decodeDate)


decodeInsert : D.Decoder InsertPayload
decodeInsert =
    D.map3 InsertPayload
        (D.field "exercise" decodeExercise)
        (D.field "order" D.int)
        (D.field "day" (D.map numberToWeekday <| D.int))


decodePayload : String -> D.Decoder Action
decodePayload action =
    case action of
        "InsertNewExercise" ->
            D.map Insert decodeInsert

        "LogSet" ->
            D.map3 (\l i s -> LogSet { loggedOn = l, number = i, set = s })
                (D.field "loggedOn" decodeDate)
                (D.field "index" D.int)
                (D.field "set" decodeSet)

        "DeleteExercise" ->
            D.map Delete decodeDelete

        "EditExercise" ->
            D.map ChangeSetsAndReps (D.list decodeSet) |> D.map Edit

        "ChangeExerciseName" ->
            D.map ChangeName (D.field "newName" D.string) |> D.map Edit

        _ ->
            D.fail ("Failed to decode this action: " ++ action)


decodeDate : D.Decoder Date
decodeDate =
    let
        handleError =
            \res ->
                case res of
                    Ok date ->
                        D.succeed date

                    Err str ->
                        D.fail str
    in
    D.string
        |> D.map Date.fromIsoString
        |> D.andThen handleError


encodeInsertRequest : InsertPayload -> E.Value
encodeInsertRequest request =
    E.object
        [ ( "action_type", E.string "InsertNewExercise" )
        , ( "payload"
          , E.object
                [ ( "order", E.int request.order )
                , ( "day", E.int (weekdayToNumber request.day) )
                , ( "exercise", encodeExercise request.exercise )
                ]
          )
        ]


encodeLogExercise : String -> Date -> List StrengthSet -> E.Value
encodeLogExercise id date sets =
    E.object
        [ ( "action_type", E.string "LogExercise" )
        , ( "payload"
          , E.object
                [ ( "loggedOn", E.string <| Date.toIsoString date )
                , ( "sets", E.list encodeSet sets )
                ]
          )
        , ( "exercise_id", E.string id )
        ]


encodeLogSet : LogSetRequest -> E.Value
encodeLogSet { exerciseId, set, loggedOn, index } =
    E.object
        [ ( "action_type", E.string "LogSet" )
        , ( "payload"
          , E.object
                [ ( "loggedOn", E.string <| Date.toIsoString loggedOn )
                , ( "index", E.int index )
                , ( "set", encodeSet set )
                ]
          )
        , ( "exercise_id", E.string exerciseId )
        ]


encodeDeleteRequest : String -> DeleteExerciseRequest -> E.Value
encodeDeleteRequest exerciseId request =
    E.object
        [ ( "action_type", E.string "DeleteExercise" )
        , ( "payload"
          , E.object
                [ ( "asOfDate", E.string (Date.toIsoString request.asOf) )
                ]
          )
        , ( "exercise_id", E.string exerciseId )
        ]


encodeEditRequest : String -> ChangeExercise -> E.Value
encodeEditRequest exerciseId changeRequest =
    let
        ( action, payload ) =
            case changeRequest of
                ChangeName newName ->
                    ( "ChangeExerciseName", E.object [ ( "newName", E.string newName ) ] )

                ChangeSetsAndReps newSets ->
                    ( "EditExercise", E.list encodeSet newSets )
    in
    E.object
        [ ( "action_type", E.string action )
        , ( "payload", payload )
        , ( "exercise_id", E.string exerciseId )
        ]



-- Types --


type alias LogSetRequest =
    { exerciseId : String
    , set : StrengthSet
    , loggedOn : Date
    , index : Int
    }


type alias DeleteExerciseRequest =
    { asOf : Date
    }


type alias InsertPayload =
    { exercise : StrengthExercise
    , order : Int
    , day : Weekday
    }


type ChangeExercise
    = ChangeName String
    | ChangeSetsAndReps (List StrengthSet)


type Action
    = Insert InsertPayload
    | LogSet { loggedOn : Date, set : StrengthSet, number : Int }
    | LogExercise { loggedOn : Date, sets : List StrengthSet }
    | Delete DeleteExerciseRequest
    | Edit ChangeExercise


type alias JournalRow =
    { exerciseId : String
    , sequenceNumber : Int
    , timestamp : Time.Posix
    , deleted : Bool
    , action : Action
    }


type alias RawJournalRow =
    { exerciseId : String
    , sequenceNumber : Int
    , timestamp : Time.Posix
    , deleted : Bool
    , actionType : String
    , action : D.Value
    }
