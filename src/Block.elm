module Block exposing (..)

import Grid

import Color exposing (..)
import Random
import Collage

type alias Block =
    { x : Int
    , y : Int
    , grid : Grid.Grid
    }


makeBlock : Int -> Color -> List (List Int) -> Block
makeBlock startX color thing =
    let
        grid = flip List.map thing <| List.map (\x -> (x, color))
    in
        Block startX 0 <| Grid.fromListsOfCells grid


iBlock : Block
iBlock =
    makeBlock 3 red [ List.range 0 3 ]


jBlock : Block
jBlock =
    makeBlock 4 orange [ [ 0, 1, 2 ] , [ 2 ] ]


lBlock : Block
lBlock =
    makeBlock 4 purple [ [ 0, 1, 2 ], [ 0 ] ]


oBlock : Block
oBlock =
    makeBlock 5 blue [ [ 0, 1 ], [ 0, 1 ] ]


sBlock : Block
sBlock =
    makeBlock 5 green [ [ 1, 2 ], [ 0, 1 ] ]


tBlock : Block
tBlock =
    makeBlock 4 darkGreen [ [ 0, 1, 2 ], [ 1 ] ]


zBlock : Block
zBlock =
    makeBlock 5 brown [ [ 0, 1 ], [ 1, 2 ] ]


availableBlocks : List Block
availableBlocks =
    [ iBlock
    , jBlock
    , lBlock
    , oBlock
    , sBlock
    , tBlock
    , zBlock
    ]


getRandomBlock : Random.Seed -> (Random.Seed, Block)
getRandomBlock seed =
    let
        -- Todo, get initial seed from random command
        generator = Random.int 0 <| List.length availableBlocks - 1
        (random, newSeed) = Random.step generator seed
    in
        List.drop random availableBlocks
            |> List.head
            |> Maybe.withDefault oBlock
            |> (,) newSeed


blockWidth : Block -> Int
blockWidth { grid } =
    Grid.width grid


blockHeight : Block -> Int
blockHeight { grid } =
    Grid.height grid


toGrid : Block -> Grid.Grid
toGrid block =
    Grid.mapCells (Tuple.mapFirst ((+) block.x)) block.grid

move : Int -> Int -> Int -> Block -> Block
move minX maxX dx block =
    let
        maxStartX = maxX - (blockWidth block)
        newX = clamp minX maxStartX <| block.x + dx
    in
        { block | x = newX }

rotate : Block -> Block
rotate block =
    { block | grid = Grid.rotate block.grid }


render : Block -> Collage.Form
render block =
    let
        xOffset = (toFloat (block.x * Grid.cellSize))
        yOffset = (toFloat (block.y * Grid.cellSize))
    in
        Grid.render xOffset yOffset block.grid

