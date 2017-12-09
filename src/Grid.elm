module Grid exposing
    ( Grid
    , cellSize
    , copyOnto
    , detectCollision
    , dimensions
    , empty
    , fromListsOfCells
    , height
    , mapCells
    , removeFullRows
    , render
    , rotate
    , width
    )

import Collage
import Color exposing (Color, gray)
import Transform

type alias Cell = (Int, Color)


type Grid = Grid Int Int (List (List Cell))


cellSize : Int
cellSize = 25


empty : Int -> Int -> Grid
empty cols rows =
    Grid cols rows <| List.repeat rows []


fromListsOfCells : List (List Cell) -> Grid
fromListsOfCells list =
    let
        maximumWithDefault default = Maybe.withDefault default << List.maximum

        rowWidth = maximumWithDefault 0 << List.map Tuple.first
        cols = maximumWithDefault 0 <| List.map ((+) 1) <| List.map rowWidth list
        rows = List.length list
    in
        Grid cols rows list


padGrid : Grid -> Grid
padGrid (Grid cols rows grid) =
    if List.length grid == rows then
        Grid cols rows grid
    else
        padGrid <| Grid cols rows <| [] :: grid


removeFullRows : Grid -> (Int, Grid)
removeFullRows (Grid cols rows grid) =
    let
        newGrid = List.filter (\l -> List.length l /= cols) grid
        removed = rows - (List.length newGrid)
    in
        (removed, padGrid <| Grid cols rows newGrid)


copyOnto : Int -> Grid -> Grid -> Grid
copyOnto yOffset (Grid _ sourceRows source) (Grid tcols trows target) =
    let
        aboveRows = List.take yOffset target
        fromBlockRows = List.drop yOffset target
        belowRows = List.drop sourceRows fromBlockRows

        replacedRows = fromBlockRows
                        |> List.take sourceRows
                        |> List.map2 List.append source
    in
        Grid tcols trows <| List.concat [aboveRows, replacedRows, belowRows]


detectCollision : Int -> Grid -> Grid -> Bool
detectCollision yOffset (Grid _ _ grid1) (Grid cols2 _ grid2) =
    let
        stopRow =
            List.range 0 cols2 |> List.map (flip (,) gray)

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
mapCells fn (Grid cols rows grid) =
    let
        mapCellsInRow row = List.map fn row
    in
        Grid cols rows <| List.map mapCellsInRow grid


height : Grid -> Int
height (Grid _ rows grid) =
    rows


width : Grid -> Int
width (Grid cols _ grid) =
    cols


rotate : Grid -> Grid
rotate (Grid cols rows grid) =
    let
        -- Needs refactoring
        flattened = List.indexedMap (\y cellList -> List.map (\cell -> ((Tuple.first cell, y), Tuple.second cell)) cellList) (List.reverse grid)
                        |> List.concat

        maybeMapYToX row ((x, y), c) =
            if row == x then
                Just (y, c)
            else
                Nothing

        newGrid = List.range 0 (cols - 1)
                    |> List.map (\row -> List.filterMap (maybeMapYToX row) flattened)
    in
        Grid rows cols newGrid


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
render xOffset yOffset (Grid _ _ lines) =
    renderLines xOffset yOffset lines []
        |> Collage.group


dimensions : Grid -> (Int, Int)
dimensions grid =
    (cellSize * width grid, cellSize * height grid)