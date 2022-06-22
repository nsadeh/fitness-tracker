module Api.Supabase exposing (..)

import Task exposing (Task)
import Utils.Error exposing (RequestError)


type alias Url =
    String

url: Url
url = "https://zmwmosaxfkywgkueembd.supabase.co"

key: ApiKey
key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inptd21vc2F4Zmt5d2drdWVlbWJkIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDUwNzExNTMsImV4cCI6MTk2MDY0NzE1M30.M3AI9OxwlNk97FoieuitzpBCCvVr7RDiCPtaXUQo6gM"

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

