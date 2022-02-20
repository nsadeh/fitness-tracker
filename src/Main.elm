module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Log exposing (LogType(..), log)
import User exposing (LoginInfo)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { email = "", password = "" }, Cmd.none )
        , view = \_ -> view viewLanding
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model --


url : String
url =
    "https://zmwmosaxfkywgkueembd.supabase.co"


token : String
token = ""

login : LoginInfo -> Cmd Msg
login =
    \info -> User.login url token info |> Cmd.map LoggedIn


type alias UserId =
    String


type alias Model =
    User.LoginInfo



-- Update --


noCommand : model -> ( model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


type alias Password =
    String


type Msg
    = LoginRequested
    | LoggedIn String
    | LogoutRequested
    | EmailEntered String
    | PasswordEntered Password
    | SignupRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailEntered email ->
            { model | email = email } |> noCommand

        PasswordEntered pw ->
            { model | password = pw } |> noCommand

        LoginRequested ->
            ( model, login model )

        LoggedIn result ->
            log Info result model

        LogoutRequested ->
            model |> noCommand

        SignupRequested ->
            log Debug "We are not taking new sign ups currently. Message nnnsadeh@gmail.com for an account" model



-- View --


view : Html Msg -> Document Msg
view html =
    { title = "Fit.app"
    , body = List.singleton html
    }


viewLanding : Html Msg
viewLanding =
    div [ class "login-form" ]
        [ div [ class "form-group container-fluid" ]
            [ div [ class "mx-auto" ]
                [ h1 [ class "mx-auto" ] [ text "Welcome to Fit.app!" ]
                ]
            , div [ class "container-fluid", style "margin" "15px" ]
                [ input [ type_ "email", class "form-control", placeholder "Email", style "margin-bottom" "10px", onInput EmailEntered ] []
                , input [ type_ "password", class "form-control", placeholder "Password", onInput PasswordEntered ] []
                ]
            , div [ class "container-fluid d-flex flex-row justify-content-center" ]
                [ button [ type_ "button", class "mr-2 btn btn-outline-dark btn-lg", onClick LoginRequested ] [ text "Log in" ]
                , button [ type_ "button", class "ml-2 btn btn-primary btn-lg" ] [ text "Sign up" ]
                ]
            ]
        ]
