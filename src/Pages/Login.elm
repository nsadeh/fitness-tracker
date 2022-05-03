module Pages.Login exposing (..)

import Api.Supabase exposing (AuthenticatedUser, RequestError, key, url)
import Api.User exposing (LoginInfo, api, setEmail, setPassword)
import Browser.Navigation exposing (Key)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Utils.Log exposing (LogType(..), log)


type alias Model =
    { info : LoginInfo
    , navKey : Key
    }


empty : Key -> Model
empty key =
    { info = Api.User.empty
    , navKey = key
    }



-- UPDATE --


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedLogin
    | LoginFailed RequestError
    | LoginSucceeded AuthenticatedUser
    | SubmittedRegistration


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredEmail email ->
            ( { model | info = setEmail email model.info }, Cmd.none )

        EnteredPassword password ->
            ( { model | info = setPassword password model.info }, Cmd.none )

        SubmittedLogin ->
            ( { model | info = Api.User.empty }, login model.info )

        LoginFailed _ ->
            ( { model | info = Api.User.empty }, Cmd.none )

        LoginSucceeded _ ->
            log Info "This never gets called lol" model

        SubmittedRegistration ->
            log Info "Not taking new users" model



-- let
--     tray = Toast.tray
--     toast = Toast.add tray (Toast.expireIn 2000 "We are currently not accepting new users. Please email nnnsadeh@gmail.com to be let in")
--     updateToast = Toast.update Toast.Msg tray
-- in
-- log Info "We are currently not accepting new users. Please email nnnsadeh@gmail.com to be let in" model


login : LoginInfo -> Cmd Msg
login info =
    let
        userApi =
            api url key

        response =
            userApi.login info

        toLoginCommand =
            \resp ->
                case resp of
                    Ok user ->
                        LoginSucceeded user

                    Err error ->
                        LoginFailed error
    in
    Task.attempt toLoginCommand response


printError : Http.Error -> String
printError error =
    case error of
        Http.BadUrl u ->
            "Url " ++ u ++ " returned bad URL"

        Http.BadBody body ->
            "Returned bad body for " ++ body

        Http.BadStatus statusCode ->
            "Failed with status code: " ++ String.fromInt statusCode

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Failed due to network error."



-- VIEW --


view : { title : String, content : Html Msg }
view =
    { title = "Login to Fit.app"
    , content = viewLogin
    }


viewLogin : Html Msg
viewLogin =
    div [ class "flex justify-center h-screen bg-gray-800 text-gray-100" ]
        [ div [ class "max-w-sm sm:max-w-lg" ]
            [ div [ class "pt-8 sm:pt-24 sm:justify-start justify-center" ]
                [ p [ class "text-4xl sm:text-left text-center" ] [ text "Welcome to Fit.app!" ]
                ]
            , div [ class "flex flex-col justify-center p-4 sm:py-10" ]
                [ input [ class "min-w-full border-2 rounded-md p-2 m-1", placeholder "Email", onInput EnteredEmail ] []
                , input [ class "min-w-full border-2 rounded-md p-2 m-1", type_ "password", placeholder "Password", onInput EnteredPassword ] []
                ]
            , div [ class "flex justify-center"]
                [ button [ class "border-2 border-blue-400 w-24 rounded-md m-2 p-2", type_ "button", onClick SubmittedLogin ] [ text "Log in" ]
                , button [ class "border-2 border-blue-50 w-24 rounded-md m-2 p-2", type_ "button", onClick SubmittedRegistration ] [ text "Sign up" ]
                ]
            ]
        ]
