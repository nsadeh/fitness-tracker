port module Api.User exposing (..)

import Api.Supabase exposing (AuthenticatedUser, UnauthenticatedRequest)
import Http as H exposing (request)
import Json.Decode as D
import Json.Encode as E


type alias LoginInfo =
    { email : String
    , password : String
    }

type RefreshedUser = RefreshedUser AuthenticatedUser


type alias Url =
    String


type alias ApiKey =
    String



-- Functions --


encode : LoginInfo -> E.Value
encode loginInfo =
    E.object
        [ ( "email", E.string loginInfo.email )
        , ( "password", E.string loginInfo.password )
        ]


encodeUser : AuthenticatedUser -> E.Value
encodeUser user =
    E.object
        [ ( "user_id", E.string user.userId )
        , ( "access_token", E.string user.authToken )
        , ( "refresh_token", E.string user.refreshToken )
        ]


decodeUser : D.Decoder AuthenticatedUser
decodeUser =
    D.map3 AuthenticatedUser
        (D.at [ "user", "id" ] D.string)
        (D.field "access_token" D.string)
        (D.field "refresh_token" D.string)


decodeFromLS : D.Decoder AuthenticatedUser
decodeFromLS =
    D.map3 AuthenticatedUser
        (D.field "user_id" D.string)
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
        , expect = H.expectJson identity decodeUser
        , timeout = Nothing
        , tracker = Nothing
        }

refresh : UnauthenticatedRequest String AuthenticatedUser
refresh url key refreshToken = H.request
    { method = "POST"
    , headers = 
        [ H.header "apikey" key
        , H.header "Content-Type" "application/json"
        ]
    , url = url ++ "/auth/v1/token?grant_type=refresh_token"
    , body = H.jsonBody (E.object (List.singleton ("refresh_token", E.string refreshToken)))
    , expect = H.expectJson identity decodeUser
    , timeout = Nothing
    , tracker = Nothing
    }

storeUser : AuthenticatedUser -> Cmd msg
storeUser user =
    encodeUser user
        |> setStorage

getUser: E.Value -> Result D.Error AuthenticatedUser
getUser flags = D.decodeValue decodeFromLS flags
    


port setStorage : E.Value -> Cmd msg
