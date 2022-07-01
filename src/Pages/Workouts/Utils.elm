module Pages.Workouts.Utils exposing (..)

import Date exposing (Date, Unit(..))


prevDay : Date -> Date
prevDay date =
    Date.add Days -1 date


nextDay : Date -> Date
nextDay date =
    Date.add Days 1 date


dateToString : Date -> String
dateToString date =
    Date.format "EEE, MMMM d" date

smallDateToString : Date -> String
smallDateToString date = Date.format "EEE, MM/d" date


padLists : List a -> List b -> a -> b -> ( List a, List b )
padLists la lb a b =
    let
        paddedLa =
            if List.length la < List.length lb then
                List.repeat (List.length lb - List.length la) a
                    |> List.append la

            else
                la

        paddedLb =
            if List.length la > List.length lb then
                List.repeat (List.length la - List.length lb) b
                    |> List.append lb

            else
                lb
    in
    ( paddedLa, paddedLb )
