module Api.Exercises exposing (..)

import Api.Supabase exposing (ApiKey, AuthenticatedRequest, AuthenticatedUser, Url)
import Http as H
import Json.Decode as D
import StrengthSet exposing (StrengthExercise)
import Utils.OrderedDict exposing (OrderedDict)
import Workout exposing (Workout)
import WorkoutCreator exposing (emptyForm)


type alias API =
    { get : () -> Cmd (Result H.Error Workout)
    }


api : Url -> ApiKey -> AuthenticatedUser -> API
api url key user =
    { get = getExercises url key user
    }


getExercises : AuthenticatedRequest () Workout
getExercises _ _ _ _ =
    Cmd.none
    |> Cmd.map (\_ -> Result.Ok dummy)

dummy : Workout
dummy = { exercises = mondayWorkout
    , creator = emptyForm
    }


-- H.request
--     { method = "POST"
--     , headers =
--         [ H.header "apikey" apikey
--         , H.header "Authorization" ("Bearer " ++ user.authToken)
--         ]
--     , url = url ++ "fillmein"
--     , body = H.emptyBody
--     , expect = H.expectJson identity decodeWorkout
--     , timeout = Nothing
--     , tracker = Nothing
--     }


mondayWorkout : OrderedDict String StrengthExercise
mondayWorkout =
    List.map toTuple
        [ { name = "Bench Press", sets = [ { reps = 5, weight = 150 }, { reps = 5, weight = 160 }, { reps = 5, weight = 170 } ], expanded = False }
        , { name = "Back Squat", sets = [ { reps = 10, weight = 195 }, { reps = 10, weight = 195 }, { reps = 10, weight = 195 } ], expanded = False }
        , { name = "Overhead Press", sets = [ { reps = 8, weight = 100 }, { reps = 10, weight = 95 }, { reps = 12, weight = 90 } ], expanded = False }
        ]
        |> Utils.OrderedDict.fromList


toTuple : StrengthExercise -> ( String, StrengthExercise )
toTuple ex =
    ( ex.name, ex )



-- Decoders/encoders --
-- decodeWorkout : D.Decoder Workout
-- decodeWorkout = D.map3 Workout
--     (D.field "dummy1" D.dict)
--     (D.field "dummy1" D.string)
