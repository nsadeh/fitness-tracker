module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, type_)
import Log exposing (LogType(..), log)
import Html.Attributes exposing (style)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( LoggedOut, Cmd.none )
        , view = \_ -> view viewLanding
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model --


type alias UserId =
    String


type Model
    = LoggedOut
    | LoggedIn UserId



-- Update --


noCommand : model -> ( model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


type alias Password =
    String


type Msg
    = LoginRequested UserId Password
    | LogoutRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        LoginRequested uid _ ->
            LoggedIn uid |> log Log.Debug ("Logging out " ++ uid)

        LogoutRequested ->
            LoggedOut |> log Log.Debug "Logging out!"



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
            , div [ class "container-fluid", style "margin" "15px"]
                [ input [ type_ "email", class "form-control", placeholder "Enter email", style "margin-bottom" "10px" ] []
                , input [ type_ "password", class "form-control", placeholder "Password" ] []
                ]
            , div [ class "container-fluid d-flex flex-row justify-content-center" ]
                [ button [ type_ "button", class "mr-2 btn btn-outline-dark btn-lg" ] [ text "Log in" ]
                , button [ type_ "button", class "ml-2 btn btn-primary btn-lg" ] [ text "Sign up" ]
                ]
            ]
        ]
