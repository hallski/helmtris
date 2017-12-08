module Block exposing
    ( Block
    , BlockManipulation
    , getRandom
    , detectCollisionInGrid
    , render
    , copyOntoGrid
    , moveOn
    , moveYOn
    , rotateOn
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


getBlock : Int -> Block
getBlock nr =
    List.drop nr availableBlocks
        |> List.head
        |> Maybe.withDefault oBlock


getRandom : Random.Generator Block
getRandom =
    Random.map getBlock <| Random.int 0 <| List.length availableBlocks - 1


toGrid : Block -> Grid.Grid
toGrid (Block block) =
    Grid.mapCells (Tuple.mapFirst ((+) block.x)) block.grid

type alias BlockManipulation = Grid.Grid -> Block -> Result String Block
moveOn : Int -> BlockManipulation
moveOn dx grid (Block block) =
    Block { block | x = block.x + dx }
        |> validateOnGrid grid


moveYOn : Int -> BlockManipulation
moveYOn dy grid (Block block) =
    Block { block | y = block.y + dy }
        |> validateOnGrid grid


rotateOn : BlockManipulation
rotateOn grid (Block block) =
    Block { block | grid = Grid.rotate block.grid }
        |> validateOnGrid grid


validateOnGrid : Grid.Grid -> Block -> Result String Block
validateOnGrid grid block =
    boardPositiveX block
        |> Result.andThen (boardWithinGridWidth grid)
        |> Result.andThen (withoutCollision grid)

boardPositiveX : Block -> Result String Block
boardPositiveX (Block block) =
    if block.x >= 0 then
        Ok <| Block block
    else
        Err "Board X is negative"


boardWithinGridWidth : Grid.Grid -> Block -> Result String Block
boardWithinGridWidth grid (Block block) =
    if (block.x + Grid.width block.grid) <= Grid.width grid then
        Ok <| Block block
    else
        Err "Block trailed off the grid"


withoutCollision : Grid.Grid -> Block -> Result String Block
withoutCollision grid block =
    if detectCollisionInGrid block grid then
        Err "Collision occurred"
    else
        Ok block

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
