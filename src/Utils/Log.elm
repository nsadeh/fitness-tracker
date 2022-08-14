port module Utils.Log exposing (LogLevel(..), log, logCmd)


type LogLevel
    = Info
    | Debug
    | Error


port logInfo : String -> Cmd msg


port logDebug : String -> Cmd msg


port logError : String -> Cmd msg


log : LogLevel -> String -> a -> ( a, Cmd msg )
log type_ message a =
    case type_ of
        Info ->
            ( a, logInfo message )

        Debug ->
            ( a, logDebug message )

        Error ->
            ( a, logError message )


logCmd : LogLevel -> String -> Cmd msg
logCmd type_ message =
    case type_ of
        Info ->
            logInfo message

        Debug ->
            logDebug message

        Error ->
            logError message
