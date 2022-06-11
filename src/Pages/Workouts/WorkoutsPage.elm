module Pages.Workouts.WorkoutsPage exposing (..)

import Pages.Workouts.WorkoutsState exposing (WorkoutsPageState)


type Model
    = Unauthenticated
    | Authenticated WorkoutsPageState
