module Pages.Login exposing (..)

import Api.Supabase exposing (AuthenticatedUser, RequestError, key, url)
import Api.User exposing (LoginInfo, api)
import Browser.Navigation exposing (Key)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Utils.Log exposing (LogType(..), log)
import Api.User exposing (setEmail)
import Api.User exposing (setPassword)


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
    div [ class "login-form" ]
        [ div [ class "form-group container-fluid" ]
            [ div [ class "mx-auto" ]
                [ h1 [ class "mx-auto" ] [ text "Welcome to Fit.app!" ]
                ]
            , div [ class "container-fluid", style "margin" "15px" ]
                [ input [ type_ "email", class "form-control", placeholder "Email", style "margin-bottom" "10px", onInput EnteredEmail ] []
                , input [ type_ "password", class "form-control", placeholder "Password", onInput EnteredPassword ] []
                ]
            , div [ class "container-fluid d-flex flex-row justify-content-center" ]
                [ button [ type_ "button", class "mr-2 btn btn-outline-dark btn-lg", onClick SubmittedLogin ] [ text "Log in" ]
                , button [ type_ "button", class "ml-2 btn btn-primary btn-lg", onClick SubmittedRegistration ] [ text "Sign up" ]
                ]
            ]
        ]
