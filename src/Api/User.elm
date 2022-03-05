module Api.User exposing (..)

import Http as H exposing (request)
import Json.Decode as D
import Json.Encode
import Api.Supabase exposing (AuthenticatedUser, UnauthenticatedRequest)

type alias LoginInfo =
    { email : String
    , password : String
    }


type alias Url =
    String


type alias ApiKey =
    String



-- Functions --


encode : LoginInfo -> Json.Encode.Value
encode loginInfo =
    Json.Encode.object
        [ ( "email", Json.Encode.string loginInfo.email )
        , ( "password", Json.Encode.string loginInfo.password )
        ]


decode : D.Decoder AuthenticatedUser
decode =
    D.map3 AuthenticatedUser
        (D.at [ "user" , "id" ] D.string)
        (D.field "access_token" D.string)
        (D.field "refresh_token" D.string)


type alias RawUser =
    { id : String
    }


decodeId : D.Decoder RawUser
decodeId =
    D.map RawUser (D.field "id" D.string)


login : UnauthenticatedRequest LoginInfo AuthenticatedUser
login url apiKey loginInfo =
    request
        { method = "POST"
        , headers =
            [ H.header "apikey" apiKey
            ]
        , url = url ++ "/auth/v1/token?grant_type=password"
        , body = H.jsonBody (encode loginInfo)
        , expect = H.expectJson identity decode
        , timeout = Nothing
        , tracker = Nothing
        }
