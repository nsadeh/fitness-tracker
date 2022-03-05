module Pages.Login exposing (..)

import Api.Supabase exposing (AuthenticatedUser)
import Api.User exposing (LoginInfo)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Toast
import Utils.Log exposing (LogType(..), log)
import Api.Supabase exposing (url)
import Api.Supabase exposing (key)


type alias Model =
    LoginInfo


empty : Model
empty =
    { email = ""
    , password = ""
    }



-- UPDATE --


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedLogin
    | LoginFailed Http.Error
    | LoginSucceeded AuthenticatedUser
    | SubmittedRegistration


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        SubmittedLogin ->
            ( empty, login model )

        LoginFailed _ ->
            ( empty, Cmd.none )

        LoginSucceeded _ ->
            log Info "Logged in" model

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
        response =
            Api.User.login url key info

        toLoginCommand =
            \resp ->
                case resp of
                    Ok user ->
                        LoginSucceeded user

                    Err error ->
                        LoginFailed error
    in
    Cmd.map toLoginCommand response


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
