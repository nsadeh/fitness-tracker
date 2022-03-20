module Api.Exercises exposing (API, api, InsertPayload, DeleteExerciseRequest, LogSetRequest)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, RequestError(..), Url)
import Date exposing (Date, Weekday, numberToWeekday, weekdayToNumber)
import Dict exposing (Dict)
import Http as H
import Json.Decode as D
import Json.Decode.Extra exposing (..)
import Json.Encode as E
import Platform exposing (Task)
import Result as Result
import StrengthSet exposing (StrengthExercise, StrengthSet, decodeExercise, encodeExercise, encodeSet)
import Task
import Time
import Utils.OrderedDict exposing (empty, insert)
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
        |> mapTaskResult (List.foldl foldRow Dict.empty)
        |> mapTaskResult (Dict.get (weekdayToNumber day))
        |> mapTaskResult (Maybe.withDefault empty)

insertJournalEntry : AuthenticatedRequest ( String, Action ) ()
insertJournalEntry url key user ( id, action ) =
    let
        body =
            case action of
                Insert request ->
                    encodeInsertRequest request

                Delete request ->
                    encodeDeleteRequest request

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


foldRow : JournalRow -> Dict Int Workout -> Dict Int Workout
foldRow row workouts =
    case row.action of
        Insert request ->
            let
                current =
                    Dict.get (weekdayToNumber request.day) workouts
                        |> Maybe.withDefault empty
            in
            Dict.insert (weekdayToNumber request.day) (insert row.exerciseId request.exercise current) workouts

        LogSet _ ->
            workouts

        Delete _ ->
            workouts


mapTaskResult : (r -> s) -> Task RequestError r -> Task RequestError s
mapTaskResult mapper task =
    Task.map mapper task

-- Resolvers -- 

resolveNothing : H.Response String -> Result RequestError ()
resolveNothing response =
    case response of
        H.GoodStatus_ _ _ ->
            Ok ()

        _ ->
            Err H.NetworkError |> Result.mapError Http

resolveRow : H.Response String -> Result RequestError (List JournalRow)
resolveRow response =
    case response of
        H.GoodStatus_ _ body ->
            D.decodeString (D.list decodeRow) body |> Result.mapError Parsing

        _ ->
            Err H.NetworkError |> Result.mapError Http


-- Decoders/Encoders


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
    D.map (DeleteExerciseRequest)
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
        mapper =
            \res ->
                case res of
                    Ok date ->
                        D.succeed date

                    Err str ->
                        D.fail str
    in
    D.string
        |> D.map Date.fromIsoString
        |> D.andThen mapper


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


encodeDeleteRequest : DeleteExerciseRequest -> E.Value
encodeDeleteRequest request =
    E.object
        [ ( "action_type", E.string "DeleteExercise" )
        , ( "payload"
          , E.object
                [ ( "asOfDate", E.string (Date.toIsoString request.asOf) )
                ]
          )
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








