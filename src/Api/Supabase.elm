module Api.Supabase exposing (..)

import Http as H
import Json.Decode as D
import Task exposing (Task)


type alias Url =
    String

url: Url
url = "https://zmwmosaxfkywgkueembd.supabase.co"

key: ApiKey
key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inptd21vc2F4Zmt5d2drdWVlbWJkIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDUwNzExNTMsImV4cCI6MTk2MDY0NzE1M30.M3AI9OxwlNk97FoieuitzpBCCvVr7RDiCPtaXUQo6gM"


type RequestError
    = Parsing D.Error
    | Http H.Error

formatError: H.Response m -> Result RequestError m
formatError response = case response of 
    H.GoodStatus_ _ body -> Ok body
    H.BadUrl_ u -> Err ( Http ( H.BadUrl u))
    H.Timeout_ -> Err (Http H.Timeout )
    H.BadStatus_ metadata _ -> Err (Http (H.BadStatus metadata.statusCode))
    H.NetworkError_ -> Err (Http (H.NetworkError))

type alias ApiKey =
    String


type alias UnauthenticatedRequest input out =
    Url -> ApiKey -> input -> Task RequestError out


type alias AuthenticatedRequest input out =
    Url -> ApiKey -> AuthenticatedUser -> input -> Task RequestError out


type alias AuthenticatedUser =
    { userId : String
    , authToken : String
    , refreshToken : String
    }

