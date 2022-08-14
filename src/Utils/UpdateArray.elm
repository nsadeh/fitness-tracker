module Utils.UpdateArray exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Attributes exposing (list)
import Maybe exposing (withDefault)


updateArray : Int -> (Maybe a -> Maybe a) -> a -> Array a -> Array a
updateArray index map default array =
    Array.set index (Array.get index array |> map |> withDefault default) array


upsertDict : comparable -> (Maybe v -> Maybe v) -> v -> Dict comparable v -> Dict comparable v
upsertDict key map default dict =
    case Dict.get key dict of
        Just _ ->
            Dict.update key map dict

        Nothing ->
            Dict.insert key default dict


last : List a -> Maybe a
last list =
    List.drop (List.length list - 1) list 
        |> List.head
