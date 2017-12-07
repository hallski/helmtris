module Grid exposing
    ( Grid
    , cellSize
    , copyOnto
    , detectCollision
    , empty
    , fromListsOfCells
    , mapCells
    , removeFullRows
    , render
    , rotate
    , width
    )

import Color exposing (Color, gray)
import Collage
import Transform

type alias Cell = (Int, Color)


type Grid = Grid (List (List Cell))
cellSize : Int
cellSize = 25


empty : Int -> Grid
empty rows =
    Grid <| List.repeat rows []


fromListsOfCells : List (List Cell) -> Grid
fromListsOfCells list =
    Grid list


padGrid : Int -> Grid -> Grid
padGrid nr (Grid grid) =
    case nr of
        0 ->
            Grid grid
        n ->
            padGrid (n - 1) <| Grid <| [] :: grid


removeFullRows : Int -> Grid -> (Int, Grid)
removeFullRows fullWidth (Grid grid) =
    let
        newGrid = List.filter (\l -> List.length l /= fullWidth) grid
        removed = (List.length grid) - (List.length newGrid)
    in
        (removed, padGrid removed (Grid newGrid))


copyOnto : Int -> Grid -> Grid -> Grid
copyOnto yOffset (Grid source) (Grid target) =
    let
        sourceHeight = List.length source
        aboveRows = List.take yOffset target
        fromBlockRows = List.drop yOffset target
        belowRows = List.drop sourceHeight fromBlockRows

        replacedRows = fromBlockRows
                        |> List.take sourceHeight
                        |> List.map2 List.append source
    in
        Grid <| List.concat [aboveRows, replacedRows, belowRows]


detectCollision : Int -> Grid -> Grid -> Bool
detectCollision yOffset (Grid grid1) (Grid grid2) =
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


mapCells : (Cell -> Cell) -> Grid -> Grid
mapCells fn (Grid grid) =
    let
        mapCellsInRow row = List.map fn row
    in
        Grid <| List.map mapCellsInRow grid


height : Grid -> Int
height (Grid grid) =
    List.length grid


width : Grid -> Int
width (Grid grid) =
    let
        maximumWithDefault default =
            Maybe.withDefault default << List.maximum

        rowWidth =
             maximumWithDefault 0 << List.map Tuple.first
    in
        maximumWithDefault 0 <| List.map ((+) 1) <| List.map rowWidth grid


rotate : Grid -> Grid
rotate (Grid grid) =
    let
        -- Needs refactoring
        flattened = List.indexedMap (\y cellList -> List.map (\cell -> ((Tuple.first cell, y), Tuple.second cell)) cellList) (List.reverse grid)
                        |> List.concat

        maybeMapYToX row ((x, y), c) =
            if row == x then
                Just (y, c)
            else
                Nothing

        newGrid = List.range 0 (width (Grid grid) - 1)
                    |> List.map (\row -> List.filterMap (maybeMapYToX row) flattened)
    in
        Grid newGrid

renderLine : List Cell -> List Collage.Form -> List Collage.Form
renderLine line rendered =
    case line of
        [] ->
            rendered

        (pos, color) :: rest ->
            let
                renderedCell = Collage.rect (toFloat cellSize) (toFloat cellSize)
                                |> Collage.filled color
                                |> Collage.move ((toFloat (pos * cellSize)), 0)
            in
                renderLine rest <| renderedCell :: rendered


renderLines : Float -> Float -> List (List Cell) -> List Collage.Form -> List Collage.Form
renderLines xOffset yOffset lines rendered =
    case lines of
        [] ->
            rendered

        line :: rest ->
            let
                renderedLine =
                    renderLine line []
                        |> Collage.groupTransform (Transform.translation xOffset yOffset)
            in
                renderLines xOffset (yOffset + (toFloat cellSize)) rest <| renderedLine :: rendered


render : Float -> Float -> Grid -> Collage.Form
render xOffset yOffset (Grid lines) =
    renderLines xOffset yOffset lines []
        |> Collage.group