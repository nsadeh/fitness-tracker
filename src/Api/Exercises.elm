module Api.Exercises exposing (API, DeleteExerciseRequest, InsertPayload, LogSetRequest, api)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, Url)
import Array
import Date exposing (Date, Weekday, numberToWeekday, weekdayToNumber)
import Http as H
import Json.Decode as D
import Json.Decode.Extra exposing (..)
import Json.Encode as E
import Pages.Workouts.ExerciseBuilder exposing (Msg(..))
import Platform exposing (Task)
import Result as Result
import StrengthSet as Strength exposing (LoggableStrengthExercise, StrengthExercise, StrengthSet, decodeExercise, decodeSet, encodeExercise, encodeSet)
import Task
import Time
import Utils.Error exposing (RequestError(..), responseResult)
import Utils.Log exposing (LogLevel(..))
import Utils.OrderedDict exposing (OrderedDict, empty, filter, insert, map, remove, swap, update)


{-| Exercises API

This is an API to interact with the workouts data

-}
type alias ExerciseId =
    String


type alias API =
    { getLoggedWorkouts : Date -> Task RequestError (OrderedDict String LoggableStrengthExercise)
    , insert : Date -> InsertPayload -> Task RequestError ()
    , logSet : ExerciseId -> Date -> Int -> StrengthSet -> Task RequestError ()
    , logExercise : Date -> ExerciseId -> List StrengthSet -> Task RequestError ()
    , deleteExercise : Date -> ExerciseId -> Task RequestError ()
    , editSets : Date -> ExerciseId -> List StrengthSet -> Task RequestError ()
    , editName : Date -> ExerciseId -> String -> Task RequestError ()
    , getExercise : Date -> ExerciseId -> Task RequestError (Maybe LoggableStrengthExercise)
    , swapExercises : Date -> ExerciseId -> ExerciseId -> Task RequestError ()
    }


api : Url -> ApiKey -> AuthenticatedUser -> API
api url key user =
    { getLoggedWorkouts = getToday foldLoggedRow url key user
    , insert = \d p -> insertJournalEntry url key user ( "", Insert d p )
    , logSet = \id loggedOn index set -> insertJournalEntry url key user ( id, LogSet loggedOn set index )
    , deleteExercise = \date id -> insertJournalEntry url key user ( id, Delete date )
    , editSets = \date id sets -> insertJournalEntry url key user ( id, Edit date (ChangeSetsAndReps sets) )
    , editName = \date id name -> insertJournalEntry url key user ( id, Edit date (ChangeName name) )
    , logExercise = \date id sets -> insertJournalEntry url key user ( id, LogExercise date sets )
    , getExercise = \date id -> getExerciseByID foldLoggedRowForExercise url key user { date = date, id = id }
    , swapExercises = \date id with -> insertJournalEntry url key user ( id, SwapExercises date with )
    }



-- Implementation --


getToday : (JournalRow -> OrderedDict String ( Int, m ) -> OrderedDict String ( Int, m )) -> AuthenticatedRequest Date (OrderedDict String m)
getToday parser url key user date =
    let
        rows =
            getJournal url key user date

        day =
            Date.weekday date
    in
    rows
        |> mapTaskResult (List.foldl parser empty)
        |> mapTaskResult (filter (\_ tuple -> Tuple.first tuple == weekdayToNumber day))
        |> mapTaskResult (map (\_ tuple -> Tuple.second tuple))


getExerciseByID : (JournalRow -> Maybe LoggableStrengthExercise -> Maybe LoggableStrengthExercise) -> AuthenticatedRequest { date : Date, id : ExerciseId } (Maybe LoggableStrengthExercise)
getExerciseByID reducer url key user { date, id } =
    getJournalForID url key user { date = date, id = id }
        |> mapTaskResult (List.foldl reducer Nothing)


getJournal : AuthenticatedRequest Date (List JournalRow)
getJournal url key user date =
    H.task
        { method = "GET"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal?user_id=eq." ++ user.userId ++ "&deleted=eq.false" ++ "&as_of=lte." ++ Date.toIsoString date ++ "&order=ordering"
        , body = H.emptyBody
        , resolver = H.stringResolver resolveRow
        , timeout = Nothing
        }


getJournalForID : AuthenticatedRequest { date : Date, id : ExerciseId } (List JournalRow)
getJournalForID url key user { date, id } =
    H.task
        { method = "GET"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal?user_id=eq." ++ user.userId ++ "&deleted=eq.false" ++ "&exercise_id=eq." ++ id ++ "&as_of=lte." ++ Date.toIsoString date ++ "&order=ordering"
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
                Insert date request ->
                    encodeInsertRequest date request

                Delete date ->
                    encodeDeleteRequest id date

                LogSet loggedOn set number ->
                    encodeLogSet { exerciseId = id, loggedOn = loggedOn, set = set, index = number }

                Edit date request ->
                    encodeEditRequest id date request

                LogExercise loggedOn sets ->
                    encodeLogExercise id loggedOn sets

                SwapExercises date with ->
                    encodeSwapExercise date id with
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


foldLoggedRowForExercise : JournalRow -> Maybe LoggableStrengthExercise -> Maybe LoggableStrengthExercise
foldLoggedRowForExercise row exercise =
    case ( exercise, row.action ) of
        ( Nothing, Insert _ insert ) ->
            Strength.make insert.exercise.name
                (insert.exercise.sets
                    |> Array.map Strength.asLoggable
                )
                |> Just

        ( Nothing, _ ) ->
            Nothing

        ( Just ex, action ) ->
            case action of
                LogSet _ _ _ ->
                    Just ex

                Insert _ _ ->
                    Just ex

                LogExercise _ _ ->
                    Just ex

                Delete _ ->
                    Nothing

                Edit _ edit ->
                    case edit of
                        ChangeName name ->
                            ex
                                |> Strength.withName name
                                |> Just

                        ChangeSetsAndReps sets ->
                            ex
                                |> Strength.updateSets (List.map Strength.asLoggable sets |> Array.fromList)
                                |> Just

                SwapExercises _ _ ->
                    Just ex


foldLoggedRow : JournalRow -> OrderedDict String ( Int, LoggableStrengthExercise ) -> OrderedDict String ( Int, LoggableStrengthExercise )
foldLoggedRow row workouts =
    case row.action of
        Insert _ insertRequest ->
            let
                exercise =
                    Strength.make insertRequest.exercise.name
                        (insertRequest.exercise.sets
                            |> Array.map Strength.asLoggable
                        )
            in
            insert row.exerciseId ( weekdayToNumber insertRequest.day, exercise ) workouts

        -- deprecated --
        LogSet _ _ _ ->
            workouts

        Delete _ ->
            remove row.exerciseId workouts

        Edit _ cr ->
            case cr of
                ChangeName name ->
                    update row.exerciseId (Tuple.mapSecond (Strength.withName name) |> Maybe.map) workouts

                ChangeSetsAndReps newSets ->
                    update row.exerciseId
                        (Maybe.map <|
                            Tuple.mapSecond
                                (Strength.updateSets (List.map Strength.asLoggable newSets |> Array.fromList))
                        )
                        workouts

        LogExercise _ _ ->
            workouts

        SwapExercises _ b ->
            swap row.exerciseId b workouts


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
    D.andThen (\raw -> rowWithAction <| decodePayload raw.asOf raw.actionType) decodeRaw


decodeRaw : D.Decoder RawJournalRow
decodeRaw =
    D.map7 RawJournalRow
        (D.field "exercise_id" D.string)
        (D.field "sequence_number" D.int)
        (D.field "timestamp" datetime)
        (D.field "deleted" D.bool)
        (D.field "action_type" D.string)
        (D.field "payload" D.value)
        (D.field "as_of" decodeDate)


rowWithAction : D.Decoder Action -> D.Decoder JournalRow
rowWithAction decoder =
    D.map6 JournalRow
        (D.field "exercise_id" D.string)
        (D.field "sequence_number" D.int)
        (D.field "timestamp" datetime)
        (D.field "deleted" D.bool)
        (D.field "payload" decoder)
        (D.field "as_of" decodeDate)


decodeInsert : D.Decoder InsertPayload
decodeInsert =
    D.map3 InsertPayload
        (D.field "exercise" decodeExercise)
        (D.field "order" D.int)
        (D.field "day" (D.map numberToWeekday <| D.int))


decodePayload : Date -> String -> D.Decoder Action
decodePayload date action =
    case action of
        "InsertNewExercise" ->
            D.map (Insert date) decodeInsert

        "LogSet" ->
            D.map3 LogSet
                (D.field "loggedOn" decodeDate)
                (D.field "set" decodeSet)
                (D.field "index" D.int)

        "DeleteExercise" ->
            D.succeed (Delete date)

        "EditExercise" ->
            D.map ChangeSetsAndReps (D.list decodeSet) |> D.map (Edit date)

        "ChangeExerciseName" ->
            D.map ChangeName (D.field "newName" D.string) |> D.map (Edit date)

        "LogExercise" ->
            D.map (LogExercise date) (D.field "sets" (D.list decodeSet))

        "SwapExercise" ->
            D.map (SwapExercises date) (D.field "with" D.string)

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


encodeInsertRequest : Date -> InsertPayload -> E.Value
encodeInsertRequest date request =
    E.object
        [ ( "action_type", E.string "InsertNewExercise" )
        , ( "payload"
          , E.object
                [ ( "order", E.int request.order )
                , ( "day", E.int (weekdayToNumber request.day) )
                , ( "exercise", encodeExercise request.exercise )
                ]
          )
        , ( "as_of", E.string <| Date.toIsoString date )
        ]


encodeSwapExercise : Date -> String -> String -> E.Value
encodeSwapExercise date id with =
    E.object
        [ ( "action_type", E.string "SwapExercise" )
        , ( "payload", E.object [ ( "with", E.string with ) ] )
        , ( "as_of", E.string <| Date.toIsoString date )
        , ( "exercise_id", E.string id )
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
        , ( "as_of", E.string <| Date.toIsoString date )
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
        , ( "as_of", E.string <| Date.toIsoString loggedOn )
        ]


encodeDeleteRequest : String -> Date -> E.Value
encodeDeleteRequest exerciseId date =
    E.object
        [ ( "action_type", E.string "DeleteExercise" )
        , ( "payload"
          , E.object
                [ ( "asOfDate", E.string (Date.toIsoString date) )
                ]
          )
        , ( "exercise_id", E.string exerciseId )
        , ( "as_of", E.string <| Date.toIsoString date )
        ]


encodeEditRequest : String -> Date -> ChangeExercise -> E.Value
encodeEditRequest exerciseId date changeRequest =
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
        , ( "as_of", E.string <| Date.toIsoString date )
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
    = Insert Date InsertPayload
    | LogSet Date StrengthSet Int
    | LogExercise Date (List StrengthSet)
    | Delete Date
    | Edit Date ChangeExercise
    | SwapExercises Date String


type alias JournalRow =
    { exerciseId : String
    , sequenceNumber : Int
    , timestamp : Time.Posix
    , deleted : Bool
    , action : Action
    , asOf : Date
    }


type alias RawJournalRow =
    { exerciseId : String
    , sequenceNumber : Int
    , timestamp : Time.Posix
    , deleted : Bool
    , actionType : String
    , action : D.Value
    , asOf : Date
    }
