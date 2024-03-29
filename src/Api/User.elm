port module Api.User exposing (API, LoginInfo, api, empty, getUser, setEmail, setPassword, storeUser)

import Api.Supabase exposing (AuthenticatedUser, UnauthenticatedRequest)
import Http as H
import Json.Decode as D
import Json.Encode as E
import Platform exposing (Task)
import Utils.Error exposing (RequestError(..), responseResult)



-- API --


type alias API =
    { login : LoginInfo -> Task RequestError AuthenticatedUser
    , refreshAuth : String -> Task RequestError AuthenticatedUser
    , changePassword : String -> String -> Task RequestError ()
    }


api : Url -> ApiKey -> API
api url apiKey =
    { login = login url apiKey
    , refreshAuth = refresh url apiKey
    , changePassword = \token password -> changePassword token url apiKey password
    }



-- Implementation --


login : UnauthenticatedRequest LoginInfo AuthenticatedUser
login url apiKey loginInfo =
    H.task
        { method = "POST"
        , headers =
            [ H.header "apikey" apiKey
            ]
        , url = url ++ "/auth/v1/token?grant_type=password"
        , body = H.jsonBody (encode loginInfo)
        , resolver = H.stringResolver userResolver
        , timeout = Nothing
        }


refresh : UnauthenticatedRequest String AuthenticatedUser
refresh url key refreshToken =
    H.task
        { method = "POST"
        , headers =
            [ H.header "apikey" key
            , H.header "Content-Type" "application/json"
            ]
        , url = url ++ "/auth/v1/token?grant_type=refresh_token"
        , body = H.jsonBody (E.object (List.singleton ( "refresh_token", E.string refreshToken )))
        , resolver = H.stringResolver userResolver
        , timeout = Nothing
        }


changePassword : String -> UnauthenticatedRequest String ()
changePassword token =
    \url key newPassword ->
        H.task
            { method = "PUT"
            , headers =
                [ H.header "apikey" key
                , H.header "Content-type" "application/json"
                , H.header "Authorization" ("Bearer " ++ token)
                ]
            , url = url ++ "/auth/v1/user"
            , body = H.jsonBody (E.object <| List.singleton ( "password", E.string newPassword ))
            , resolver = H.stringResolver <| \r -> Result.map (\_ -> ()) (responseResult r)
            , timeout = Nothing
            }



-- Decoders/Encoders --


decodeUser : D.Decoder AuthenticatedUser
decodeUser =
    D.map3 AuthenticatedUser
        (D.at [ "user", "id" ] D.string)
        (D.field "access_token" D.string)
        (D.field "refresh_token" D.string)


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



-- Resolvers --


userResolver : H.Response String -> Result RequestError AuthenticatedUser
userResolver response =
    let
        decode =
            \user ->
                D.decodeString decodeUser user
                    |> Result.mapError Parsing
    in
    responseResult response
        |> Result.andThen decode



-- Types --


type alias LoginInfo =
    { email : String
    , password : String
    }


empty =
    { email = "", password = "" }


type alias Url =
    String


type alias ApiKey =
    String



-- local storage --


storeUser : AuthenticatedUser -> Cmd msg
storeUser user =
    encodeUser user
        |> setStorage


getUser : E.Value -> Result D.Error AuthenticatedUser
getUser flags =
    D.decodeValue decodeFromLS flags


decodeFromLS : D.Decoder AuthenticatedUser
decodeFromLS =
    D.map3 AuthenticatedUser
        (D.field "user_id" D.string)
        (D.field "access_token" D.string)
        (D.field "refresh_token" D.string)


setEmail : String -> LoginInfo -> LoginInfo
setEmail email info =
    { info | email = email }


setPassword : String -> LoginInfo -> LoginInfo
setPassword password info =
    { info | password = password }


port setStorage : E.Value -> Cmd msg
