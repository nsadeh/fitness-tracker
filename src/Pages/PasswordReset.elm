module Pages.PasswordReset exposing (Model, Msg(..), fromToken, update, view)

import Api.Supabase exposing (key, url)
import Api.User exposing (api)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Task
import Utils.Error exposing (handle)



--Model --


type alias Args =
    { top : String
    , bottom : String
    , token : String
    }


type Model
    = PasswordReset Args


fromToken : String -> Model
fromToken token =
    PasswordReset
        { top = ""
        , bottom = ""
        , token = token
        }



-- isValidPassword : Args -> Bool
-- isValidPassword model =
--     case model of
--         { top, bottom } ->
--             top == bottom
-- Update --


type Msg
    = EnterTopPassword String
    | EnterBottomPassword String
    | SetToken String
    | Confirm
    | ConfirmationSucceeded
    | ConfirmationFailed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (PasswordReset model) =
    case msg of
        EnterBottomPassword password ->
            ( PasswordReset { model | bottom = password }, Cmd.none )

        EnterTopPassword password ->
            ( PasswordReset { model | top = password }, Cmd.none )

        SetToken token ->
            ( PasswordReset { model | token = token }, Cmd.none )

        Confirm ->
            let
                userApi =
                    api url key
            in
            ( PasswordReset model, Task.attempt (handle (\_ -> ConfirmationSucceeded) (\_ -> ConfirmationFailed)) (userApi.changePassword model.token model.bottom) )

        ConfirmationSucceeded ->
            ( PasswordReset model, Cmd.none )

        ConfirmationFailed ->
            ( PasswordReset model, Cmd.none )



-- View --


view : Model -> Html Msg
view (PasswordReset args) =
    div [ class "w-screen h-screen bg-gray-900 flex justify-center" ]
        [ div [ class "flex flex-col w-3/4 h-screen" ]
            [ div [ class "flex mt-16 mb-4 font-bold text-4xl text-gray-200" ]
                [ text "Set new password" ]
            , div [ class "flex flex-col justify-center max-h-4xl w-full border-2 border-blue-400 rounded-md" ]
                [ input [ class "mt-8 mb-6 mx-12 bg-gray-200 text-gray-800", onInput EnterTopPassword, value args.top ] []
                , input [ class "mb-6 mx-12 bg-gray-200 text-gray-800", onInput EnterBottomPassword, value args.bottom ] []
                , div [ class "flex ustify-center w-fit h-fit border-2 border-blue-400 rounded md text-gray-200 py-2 px-4 mx-auto mb-6", onClick Confirm ]
                    [ text "Confirm" ]
                ]
            ]
        ]
