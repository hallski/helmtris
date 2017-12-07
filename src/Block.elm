module Block exposing (..)

import Grid exposing (Grid)
import Helpers exposing (maximumWithDefault)

import Color exposing (..)
import Random

type alias Block =
    { x : Int
    , y : Int
    , grid : Grid
    }


makeBlock : Int -> Color -> List (List Int) -> Block
makeBlock startX color thing =
    let
        grid = flip List.map thing <| List.map (\x -> (x, color))
    in
        Block startX 0 grid


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
    let
        rowWidth =
             maximumWithDefault 0 << List.map Tuple.first
    in
        maximumWithDefault 0 <| List.map ((+) 1) <| List.map rowWidth grid


blockHeight : Block -> Int
blockHeight { grid } =
    List.length grid


toGrid : Block -> Grid
toGrid block =
    block.grid
        |> List.map (\l -> List.map (Tuple.mapFirst ((+) block.x)) l)

move : Int -> Int -> Int -> Block -> Block
move minX maxX dx block =
    let
        maxStartX = maxX - (blockWidth block)
        newX = clamp minX maxStartX <| block.x + dx
    in
        { block | x = newX }

rotate : Block -> Block
rotate block =
    let
        -- Needs refactoring
        flattened = List.indexedMap (\y cellList -> List.map (\cell -> ((Tuple.first cell, y), Tuple.second cell)) cellList) (List.reverse block.grid)
                        |> List.concat

        maybeMapYToX row ((x, y), c) =
            if row == x then
                Just (y, c)
            else
                Nothing

        newGrid = List.range 0 (blockWidth block - 1)
                    |> List.map (\row -> List.filterMap (maybeMapYToX row) flattened)
    in
        { block | grid = newGrid }

