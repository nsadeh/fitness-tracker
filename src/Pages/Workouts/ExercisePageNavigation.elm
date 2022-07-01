module Pages.Workouts.ExercisePageNavigation exposing (Action(..), navigatePage)

import Browser.Navigation as Nav
import Date exposing (Date)
import Dict
import Pages.Workouts.Utils exposing (nextDay, prevDay)
import Pages.Workouts.WorkoutsState as PageState exposing (NavbarSwipeDirection(..), WorkoutsPageState)
import StrengthSet exposing (LoggedStrengthExercise)
import Swiper
import Task exposing (Task)
import Url.Builder
import Utils.Error exposing (RequestError)
import Utils.Log exposing (LogType(..), log)
import Utils.OrderedDict exposing (OrderedDict)


type Action
    = ExpandExercise String
    | SelectDate Date
    | LoadURL Date
    | SwipedNavbar Swiper.SwipeEvent
    | SwipedExercise String Swiper.SwipeEvent
    | ImproperSelection String


navigatePage :
    { parseWorkout : Task RequestError (OrderedDict String LoggedStrengthExercise) -> Cmd msg, openEditor : String -> Cmd msg }
    -> Action
    -> WorkoutsPageState
    -> ( WorkoutsPageState, Cmd msg )
navigatePage { parseWorkout, openEditor } msg model =
    case msg of
        ExpandExercise id ->
            ( PageState.toggle model id, Cmd.none )

        SelectDate date ->
            ( PageState.updateDate model date, parseWorkout <| model.api.getLoggedWorkouts date )

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
                    navigatePage { parseWorkout = parseWorkout, openEditor = openEditor }
                        (case direction of
                            Left ->
                                LoadURL <| prevDay swipedState.today

                            Right ->
                                LoadURL <| nextDay swipedState.today
                        )
                        swipedState

        ImproperSelection err ->
            log Error err model

        SwipedExercise id event ->
            let
                ( swipedState, swipeDirection ) =
                    handleExerciseSwipe model id event
            in
            case swipeDirection of
                Nothing ->
                    ( swipedState, Cmd.none )

                Just direction ->
                    case direction of
                        Left ->
                            ( swipedState, openEditor id )

                        Right ->
                            ( swipedState, openEditor id )


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


handleExerciseSwipe : WorkoutsPageState -> String -> Swiper.SwipeEvent -> ( WorkoutsPageState, Maybe NavbarSwipeDirection )
handleExerciseSwipe state id event =
    let
        swipeState =
            Dict.get id state.exerciseSwipeState
                |> Maybe.withDefault Swiper.initialSwipingState

        ( _, swipedLeft ) =
            Swiper.hasSwipedLeft event swipeState

        ( nextState, swipedRight ) =
            Swiper.hasSwipedRight event swipeState
    in
    ( { state | exerciseSwipeState = Dict.insert id nextState state.exerciseSwipeState }
    , if swipedRight then
        Just Right

      else if swipedLeft then
        Just Left

      else
        Nothing
    )
