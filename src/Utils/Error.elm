module Utils.Error exposing (..)

import Http as H
import Json.Decode as D


type RequestError
    = Parsing D.Error
    | Navigation String
    | Http H.Error


toString : RequestError -> String
toString error =
    case error of
        Parsing parseError ->
            D.errorToString parseError |> String.append "ParseError: \n"

        Navigation navError ->
            "NavError: \n" ++ navError

        Http httpError ->
            case httpError of
                H.BadUrl url ->
                    "BadURLError: \n" ++ url

                H.BadBody body ->
                    "BadBodyError: \n" ++ body

                H.BadStatus status ->
                    String.fromInt status |> String.append "BadStatusError: \n"

                H.NetworkError ->
                    "BadNetworkError"

                H.Timeout ->
                    "TimeOutError"


responseResult : H.Response m -> Result RequestError m
responseResult response =
    case response of
        H.GoodStatus_ _ body ->
            Ok body

        H.BadUrl_ u ->
            H.BadUrl u |> Http |> Err

        H.Timeout_ ->
            H.Timeout |> Http |> Err

        H.BadStatus_ metadata _ ->
            H.BadStatus metadata.statusCode |> Http |> Err

        H.NetworkError_ ->
            H.NetworkError |> Http |> Err
