module Pages.Workouts.ExercisePageNavigation exposing (Action(..), navigatePage)

import Browser.Navigation as Nav
import Date exposing (Date)
import Pages.Workouts.Utils exposing (nextDay, prevDay)
import Pages.Workouts.WorkoutsState as PageState exposing (NavbarSwipeDirection(..), WorkoutsPageState)
import StrengthSet exposing (LoggedStrengthExercise)
import Swiper
import Task
import Url.Builder
import Utils.Error exposing (RequestError)
import Utils.OrderedDict exposing (OrderedDict)
import Utils.Log exposing (log)
import Utils.Log exposing (LogType(..))


type Action
    = ExpandExercise String
    | SelectDate Date
    | LoadURL Date
    | SwipedNavbar Swiper.SwipeEvent
    | ImproperSelection String


navigatePage :
    { parseWorkout : Result RequestError (OrderedDict String LoggedStrengthExercise) -> msg }
    -> Action
    -> WorkoutsPageState
    -> ( WorkoutsPageState, Cmd msg )
navigatePage { parseWorkout } msg model =
    case msg of
        ExpandExercise id ->
            ( PageState.toggle model id, Cmd.none )

        SelectDate date ->
            ( PageState.updateDate model date, Task.attempt parseWorkout <| model.api.getLoggedWorkouts date )

        LoadURL date ->
            ( model, changeWorkoutURL model date )

        SwipedNavbar event ->
            let
                ( swipedState, swipeDirection ) =
                    handleNavbarSwipe model event
            in
            case swipeDirection of
                Nothing ->
                    ( swipedState, Cmd.none )

                Just direction ->
                    navigatePage { parseWorkout = parseWorkout }
                        (case direction of
                            Left ->
                                LoadURL <| prevDay swipedState.today

                            Right ->
                                LoadURL <| nextDay swipedState.today
                        )
                        swipedState

        ImproperSelection err ->
            log Error err model


changeWorkoutURL : WorkoutsPageState -> Date -> Cmd msg
changeWorkoutURL state date =
    Nav.pushUrl state.navKey <| formatDateWorkoutURL date


formatDateWorkoutURL : Date -> String
formatDateWorkoutURL date =
    Url.Builder.absolute [ "workout" ] [ Url.Builder.string "date" (Date.toIsoString date) ]


handleNavbarSwipe : WorkoutsPageState -> Swiper.SwipeEvent -> ( WorkoutsPageState, Maybe NavbarSwipeDirection )
handleNavbarSwipe state event =
    let
        ( _, swipedLeft ) =
            Swiper.hasSwipedLeft event state.navbarSwipeState

        ( nextState, swipedRight ) =
            Swiper.hasSwipedRight event state.navbarSwipeState
    in
    ( { state | navbarSwipeState = nextState }
    , if swipedRight then
        Just Right

      else if swipedLeft then
        Just Left

      else
        Nothing
    )
