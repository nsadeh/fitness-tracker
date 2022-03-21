module Api.Exercises exposing (API, DeleteExerciseRequest, InsertPayload, LogSetRequest, api)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, RequestError(..), Url, formatError)
import Date exposing (Date, Weekday, numberToWeekday, weekdayToNumber)
import Http as H
import Json.Decode as D
import Json.Decode.Extra exposing (..)
import Json.Encode as E
import Platform exposing (Task)
import Result as Result
import StrengthSet exposing (StrengthExercise, StrengthSet, decodeExercise, encodeExercise, encodeSet)
import Task
import Time
import Utils.OrderedDict exposing (OrderedDict, empty, filter, insert, map, remove)
import Workout exposing (Workout)


{-| Exercises API

This is an API to interact with the workouts data

-}
type alias ExerciseId =
    String


type alias API =
    { getWorkout : Date -> Task RequestError Workout
    , insert : InsertPayload -> Task RequestError ()
    , logSet : ExerciseId -> StrengthSet -> Task RequestError ()
    , deleteExercise : ExerciseId -> Date -> Task RequestError ()
    }


api : Url -> ApiKey -> AuthenticatedUser -> API
api url key user =
    { getWorkout = getExercises url key user
    , insert = \p -> insertJournalEntry url key user ( "", Insert p )
    , logSet = \id set -> insertJournalEntry url key user ( id, LogSet set )
    , deleteExercise = \id date -> insertJournalEntry url key user ( id, Delete (DeleteExerciseRequest date) )
    }



-- Implementation --


getExercises : AuthenticatedRequest Date Workout
getExercises url key user date =
    let
        rows =
            H.task
                { method = "GET"
                , headers =
                    [ H.header "apikey" key
                    , H.header "Authorization" ("Bearer " ++ user.authToken)
                    ]
                , url = url ++ "/rest/v1/exercise_journal?user_id=eq." ++ user.userId
                , body = H.emptyBody
                , resolver = H.stringResolver resolveRow
                , timeout = Nothing
                }

        day =
            Date.weekday date
    in
    rows
        |> mapTaskResult (List.foldl foldRow empty)
        |> mapTaskResult (filter (\_ tuple -> Tuple.first tuple == weekdayToNumber day))
        |> mapTaskResult (map (\_ tuple -> Tuple.second tuple))


insertJournalEntry : AuthenticatedRequest ( String, Action ) ()
insertJournalEntry url key user ( id, action ) =
    let
        body =
            case action of
                Insert request ->
                    encodeInsertRequest request

                Delete request ->
                    encodeDeleteRequest id request

                LogSet request ->
                    encodeLogSet (LogSetRequest id request)
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


foldRow : JournalRow -> OrderedDict String ( Int, StrengthExercise ) -> OrderedDict String ( Int, StrengthExercise )
foldRow row workouts =
    case row.action of
        Insert request ->
            insert row.exerciseId ( weekdayToNumber request.day, request.exercise ) workouts

        LogSet _ ->
            workouts

        Delete _ ->
            remove row.exerciseId workouts


mapTaskResult : (r -> s) -> Task RequestError r -> Task RequestError s
mapTaskResult mapper task =
    Task.map mapper task



-- Resolvers --


resolveNothing : H.Response String -> Result RequestError ()
resolveNothing response =
    Result.map (\_ -> ()) (formatError response)


resolveRow : H.Response String -> Result RequestError (List JournalRow)
resolveRow response =
    let
        decodeJournalEntries =
            \list ->
                D.decodeString (D.list decodeRow) list
                    |> Result.mapError Parsing
    in
    formatError response
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
            D.map LogSet StrengthSet.decodeSet

        "DeleteExercise" ->
            D.map Delete decodeDelete

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
        [ ( "order", E.int request.order )
        , ( "day", E.int (weekdayToNumber request.day) )
        , ( "exercise", encodeExercise request.exercise )
        ]


encodeLogSet : LogSetRequest -> E.Value
encodeLogSet request =
    E.object
        [ ( "action_type", E.string "LogSet" )
        , ( "payload", encodeSet request.set )
        , ( "exercise_id", E.string request.exerciseId )
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
        , ("exercise_id", E.string exerciseId)
        ]



-- Types --


type alias LogSetRequest =
    { exerciseId : String
    , set : StrengthSet
    }


type alias DeleteExerciseRequest =
    { asOf : Date
    }


type alias InsertPayload =
    { exercise : StrengthExercise
    , order : Int
    , day : Weekday
    }


type Action
    = Insert InsertPayload
    | LogSet StrengthSet
    | Delete DeleteExerciseRequest


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
