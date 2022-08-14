module Actions exposing (Action(..), none, withAction, addAction)

import Date exposing (Date)


type Action
    = GoToWorkout Date
    | ResetPassword String


none : ( a, b ) -> ( a, b, List Action )
none ( a, b ) =
    ( a, b, [] )


addAction : Action -> ( a, b ) -> ( a, b, List Action )
addAction action ( a, b ) =
    ( a, b, List.singleton action )


withAction : Action -> ( a, b, List Action ) -> ( a, b, List Action )
withAction action ( a, b, ls ) =
    ( a, b, action :: ls )
