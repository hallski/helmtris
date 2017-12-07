module Block exposing
    ( Block
    , getRandom
    , detectCollisionInGrid
    , render
    , copyOntoGrid
    , move
    , moveY
    , rotate
    )

import Grid

import Collage
import Color exposing (..)
import Random


type Block = Block
    { x : Int
    , y : Int
    , grid : Grid.Grid
    }


makeBlock : Int -> Color -> List (List Int) -> Block
makeBlock startX color thing =
    let
        grid = flip List.map thing <| List.map (\x -> (x, color))
    in
        Block
            { x = startX
            , y = 0
            , grid = Grid.fromListsOfCells grid
            }


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


getRandom : Random.Seed -> (Random.Seed, Block)
getRandom seed =
    let
        -- Todo, get initial seed from random command
        generator = Random.int 0 <| List.length availableBlocks - 1
        (random, newSeed) = Random.step generator seed
    in
        List.drop random availableBlocks
            |> List.head
            |> Maybe.withDefault oBlock
            |> (,) newSeed


toGrid : Block -> Grid.Grid
toGrid (Block block) =
    Grid.mapCells (Tuple.mapFirst ((+) block.x)) block.grid


move : Int -> Int -> Int -> Block -> Block
move minX maxX dx (Block block) =
    let
        maxStartX = maxX - (Grid.width block.grid)
        newX = clamp minX maxStartX <| block.x + dx
    in
        Block { block | x = newX }


moveY : Int -> Block -> Block
moveY dy (Block block) =
    Block { block | y = block.y + dy }


rotate : Block -> Block
rotate (Block block) =
    Block { block | grid = Grid.rotate block.grid }


render : Block -> Collage.Form
render (Block block) =
    let
        xOffset = (toFloat (block.x * Grid.cellSize))
        yOffset = (toFloat (block.y * Grid.cellSize))
    in
        Grid.render xOffset yOffset block.grid


detectCollisionInGrid : Block -> Grid.Grid -> Bool
detectCollisionInGrid (Block block) grid =
    Grid.detectCollision block.y (toGrid (Block block)) grid


copyOntoGrid : Block -> Grid.Grid -> Grid.Grid
copyOntoGrid (Block block) grid =
    Grid.copyOnto block.y (toGrid (Block block)) grid
