module Grid exposing (..)

import Color exposing (Color, gray)


type alias Cell = (Int, Color)


type alias Grid = List (List Cell)


emptyGrid : Int -> Grid
emptyGrid rows =
    List.repeat rows []


padGrid : Int -> Grid -> Grid
padGrid nr grid =
    case nr of
        0 ->
            grid
        n ->
            padGrid (n - 1) <| [] :: grid


removeFullRows : Int -> Grid -> (Int, Grid)
removeFullRows fullWidth grid =
    let
        newGrid = List.filter (\l -> List.length l /= fullWidth) grid
        removed = (List.length grid) - (List.length newGrid)
    in
        (removed, padGrid removed newGrid)


copyOnto : Int -> Grid -> Grid -> Grid
copyOnto yOffset source target =
    let
        sourceHeight = List.length source
        aboveRows = List.take yOffset target
        fromBlockRows = List.drop yOffset target
        belowRows = List.drop sourceHeight fromBlockRows

        replacedRows = fromBlockRows
                        |> List.take sourceHeight
                        |> List.map2 List.append source
    in
        List.concat [aboveRows, replacedRows, belowRows]


detectCollision : Int -> Grid -> Grid -> Bool
detectCollision yOffset grid1 grid2 =
    let
        stopRow =
            List.range 0 9 |> List.map (flip (,) gray)

        gridWithStop = List.append grid2 [stopRow]
        findCollisionsInRows blockRow landedRow =
            let
                blockXs = List.map Tuple.first blockRow
                landedXs = List.map Tuple.first landedRow
            in
                List.map (flip List.member blockXs) landedXs
                    |> List.any Basics.identity
    in
        List.drop yOffset gridWithStop
            |> List.map2 findCollisionsInRows grid1
            |> List.any Basics.identity
