module Api.Exercises exposing (..)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, Url)
import Date exposing (Date, Weekday, numberToWeekday, weekdayToNumber)
import Dict exposing (Dict)
import Http as H
import Json.Decode as D
import Json.Decode.Extra exposing (..)
import Json.Encode as E
import Random exposing (generate)
import Result as Result
import StrengthSet exposing (StrengthExercise, StrengthSet, decodeExercise, encodeExercise, encodeSet)
import Time
import UUID exposing (UUID, generator)
import Utils.OrderedDict exposing (empty, insert)
import Workout exposing (Workout)


type alias API =
    { get : Date -> Cmd (Result H.Error Workout)
    , insert : InsertExerciseRequest -> Cmd (Result H.Error ())
    , logSet : String -> StrengthSet -> Cmd (Result H.Error ())
    , deleteExercise : String -> Date -> Cmd (Result H.Error ())
    }


type alias InsertExerciseRequest =
    { id : String
    , payload : InsertPayload
    }


type alias LogSetRequest =
    { exerciseId : String
    , set : StrengthSet
    }


type alias DeleteExerciseRequest =
    { exerciseId : String
    , asOf : Date
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


decodeRow : D.Decoder JournalRow
decodeRow =
    D.andThen (\raw -> rowWithAction <| decodePayload raw.actionType) decodeRaw


encodeInsertRequest : InsertExerciseRequest -> E.Value
encodeInsertRequest request =
    E.object
        [ ( "order", E.int request.payload.order )
        , ( "day", E.int (weekdayToNumber request.payload.day) )
        , ( "exercise", encodeExercise request.payload.exercise )
        ]


encodeInsertRow : String -> InsertExerciseRequest -> E.Value
encodeInsertRow userId request =
    E.object
        [ ( "user_id", E.string userId )
        , ( "action_type", E.string "InsertNewExercise" )
        , ( "payload", encodeInsertRequest request )
        , ( "exercise_id", E.string request.id )
        ]


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

        "DeletedExercise" ->
            D.map Delete 

        _ ->
            D.fail ("Failed to decode this action: " ++ action)


api : Url -> ApiKey -> AuthenticatedUser -> API
api url key user =
    { get = getExercises url key user
    , insert = \p -> insertJournalEntry url key user ( p.id, Insert p.payload )
    , logSet = \id set -> insertJournalEntry url key user ( id, LogSet set )
    , deleteExercise = \id date -> insertJournalEntry url key user ( id, Delete (DeleteExerciseRequest id date) )
    }



-- { get = getExercises url key user
-- , insert = insertExercise url key user
-- , logSet = \exerciseId set -> LogSetRequest exerciseId set |> logSet url key user
-- , deleteExercise =
--     \exerciseId asOfDate ->
--         DeleteExerciseRequest exerciseId asOfDate
--             |> deleteExercise url key user
-- }


getExercises : AuthenticatedRequest Date Workout
getExercises url key user date =
    let
        rows =
            H.request
                { method = "GET"
                , headers =
                    [ H.header "apikey" key
                    , H.header "Authorization" ("Bearer " ++ user.authToken)
                    ]
                , url = url ++ "/rest/v1/exercise_journal?user_id=eq." ++ user.userId
                , body = H.emptyBody
                , expect = H.expectJson identity (D.list decodeRow)
                , timeout = Nothing
                , tracker = Nothing
                }

        day =
            Date.weekday date
    in
    rows
        |> mapResult (List.foldl foldRow Dict.empty)
        |> mapResult (Dict.get (weekdayToNumber day))
        |> mapResult (Maybe.withDefault empty)


mapResult : (r -> s) -> Cmd (Result H.Error r) -> Cmd (Result H.Error s)
mapResult mapper cmd =
    Cmd.map (Result.map mapper) cmd


insertExercise : AuthenticatedRequest InsertExerciseRequest ()
insertExercise url key user request =
    H.request
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal"
        , body = H.jsonBody (encodeInsertRow user.userId request)
        , expect = H.expectWhatever identity
        , timeout = Nothing
        , tracker = Nothing
        }


deleteExercise : AuthenticatedRequest DeleteExerciseRequest ()
deleteExercise url key user request =
    H.request
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal"
        , body = H.jsonBody (encodeDeleteRequest request)
        , expect = H.expectWhatever identity
        , timeout = Nothing
        , tracker = Nothing
        }


logSet : AuthenticatedRequest LogSetRequest ()
logSet url key user request =
    H.request
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal"
        , body = H.jsonBody (encodeLogSet request)
        , expect = H.expectWhatever identity
        , timeout = Nothing
        , tracker = Nothing
        }


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
                , ( "exerciseId", E.string request.exerciseId )
                ]
          )
        , ( "exercise_id", E.string request.exerciseId )
        ]


insertJournalEntry : AuthenticatedRequest ( String, Action ) ()
insertJournalEntry url key user ( id, action ) =
    let
        body =
            case action of
                Insert request ->
                    encodeInsertRequest (InsertExerciseRequest id request)

                Delete request ->
                    encodeDeleteRequest request

                LogSet request ->
                    encodeLogSet (LogSetRequest id request)
    in
    H.request
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Authorization" ("Bearer " ++ user.authToken)
            ]
        , url = url ++ "/rest/v1/exercise_journal"
        , body = H.jsonBody body
        , expect = H.expectWhatever identity
        , timeout = Nothing
        , tracker = Nothing
        }


generateUUID : Cmd UUID
generateUUID =
    generate identity generator


generateExerciseID : Cmd String
generateExerciseID =
    generateUUID
        |> Cmd.map UUID.toString
        |> Cmd.map (String.append "exercise|")
