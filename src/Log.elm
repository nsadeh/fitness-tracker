port module Log exposing (log, LogType(..))

type LogType = Info | Debug | Error
port logInfo : String -> Cmd msg
port logDebug : String -> Cmd msg
port logError : String -> Cmd msg


log : LogType -> String -> a -> ( a, Cmd msg )
log type_ message a = case type_ of
   Info -> ( a, logInfo message )
   Debug -> ( a, logDebug message )
   Error -> ( a, logError message )
