module Helpers exposing (..)


maximumWithDefault : comparable -> List comparable -> comparable
maximumWithDefault default =
    Maybe.withDefault default << List.maximum



