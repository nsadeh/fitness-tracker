module User exposing (..)

import Http
import Json.Encode
import Platform exposing (Task)
import Task exposing (Task)


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


decode : Result Http.Error String -> String
decode result =
    case result of
        Ok value ->
            value

        Err _ ->
            "I messed up"

resolveLogin : Http.Response String -> Result String String
resolveLogin response = case response of
    Http.GoodStatus_ _ body -> Ok body
    _ -> Err "Fucked up" 


login : Url -> ApiKey -> LoginInfo -> Cmd String
login url apiKey loginInfo =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" apiKey
            ]
        , url = url ++ "/auth/v1/token?grant_type=password"
        , body = Http.jsonBody (encode loginInfo)
        , expect = Http.expectString decode 
        , timeout = Nothing
        , tracker = Nothing
        }



--   USER LOGIN
-- curl -X POST 'https://zmwmosaxfkywgkueembd.supabase.co/auth/v1/token?grant_type=password' \
-- -H "apikey: SUPABASE_KEY" \
-- -H "Content-Type: application/json" \
-- -d '{
--   "email": "someone@email.com",
--   "password": "uaVFNOYCDqdYXxpsSSWH"
-- }'
-- upload : File.File -> Cmd Msg
-- upload file =
--   Http.request
--     { method = "PUT"
--     , headers = []
--     , url = "https://example.com/publish"
--     , body = Http.fileBody file
--     , expect = Http.expectWhatever Uploaded
--     , timeout = Nothing
--     , tracker = Nothing
--     }
