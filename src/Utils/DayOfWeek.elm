module Utils.DayOfWeek exposing (..)

type DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

stringOfDay : DayOfWeek -> String
stringOfDay day =
    case day of
        Monday ->
            "Monday"

        Tuesday ->
            "Tuesday"

        Wednesday ->
            "Wednesday"

        Thursday ->
            "Thursday"

        Friday ->
            "Friday"

        Saturday ->
            "Saturday"

        Sunday ->
            "Sunday"

nextDay : DayOfWeek -> DayOfWeek
nextDay day =
    case day of
        Monday ->
            Tuesday

        Tuesday ->
            Wednesday

        Wednesday ->
            Thursday

        Thursday ->
            Friday

        Friday ->
            Saturday

        Saturday ->
            Sunday

        Sunday ->
            Monday

prevDay : DayOfWeek -> DayOfWeek
prevDay day =
    case day of
        Monday ->
            Sunday

        Tuesday ->
            Monday

        Wednesday ->
            Tuesday

        Thursday ->
            Wednesday

        Friday ->
            Thursday

        Saturday ->
            Friday

        Sunday ->
            Saturday