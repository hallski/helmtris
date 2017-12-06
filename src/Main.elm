module Main exposing (main)

import Color exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, on, keyCode)
import Char exposing (KeyCode)
import Collage
import Transform exposing (..)
import Element
import Time
import AnimationFrame as AF
import Keyboard as KB


gridSize : Int
gridSize = 25

playFieldSize : { cols : Int, rows : Int}
playFieldSize = { cols = 10, rows = 20 }

playFieldDimensions : { width : Int, height : Int}
playFieldDimensions =
    { width = playFieldSize.cols * gridSize
    , height = playFieldSize.rows * gridSize
    }

iBlock : Grid
iBlock =
    [ List.map (flip (,) <| 0) <| List.range 0 3 ]

jBlock : Grid
jBlock =
    [ List.map (flip (,) <| 1) <| List.range 0 2
    , [ (2, 1) ]
    ]

lBlock : Grid
lBlock =
    [ List.map (flip (,) <| 0) <| List.range 0 2
    , [ (0, 2) ]
    ]

oBlock : Grid
oBlock =
    [ [ (0, 3), (1, 3) ]
    , [ (0, 3), (1, 3) ]
    ]

sBlock : Grid
sBlock =
    [ [ (1, 4), (2, 4) ]
    , [ (0, 4), (1, 4) ]
    ]

tBlock : Grid
tBlock =
    [ List.map (flip (,) <| 0) <| List.range 0 2
    , [ (1, 5) ]
    ]

zBlock : Grid
zBlock =
    [ [ (0, 6), (1, 6) ]
    , [ (1, 6), (2, 6) ]
    ]


-- Once the blocks have landed, copy their location over to the PlayField
type alias Cell = (Int, Int)

type alias Grid = List (List Cell)

type alias Block =
    { x : Int
    , y : Int
    , grid : Grid
    }


fullRow =
    List.range 0 9
        |> List.map (\x -> (x, 10))

emptyGrid : Grid
emptyGrid =
    List.repeat playFieldSize.rows []


-- For testing
newBlock : Block
newBlock =
    Block 1 0 jBlock


type alias Model =
    { playing : Bool
    , gameOver : Bool
    , landed : Grid
    , nextDrop : Time.Time
    , dropping : Bool
    , score : Int
    , activeBlock : Block
    }


init : (Model, Cmd Msg)
init =
    ( Model False False emptyGrid 0 False 0 <| Block 3 0 iBlock, Cmd.none )


type Msg
    = Tick Time.Time
    | TogglePlay
    | Left
    | Right
    | Rotate
    | DropStart
    | DropStop
    | NoOp -- Needed by handleKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ( updateActiveBlock model time, Cmd.none )

        TogglePlay ->
            ( { model | playing = not model.playing }, Cmd.none )

        Left ->
            ( { model | activeBlock = move -1 model.activeBlock }, Cmd.none )

        Right ->
            ( { model | activeBlock = move 1 model.activeBlock }, Cmd.none )

        Rotate ->
            ( { model | activeBlock = rotate model.activeBlock }, Cmd.none )

        DropStart ->
            ( { model | dropping = True }, Cmd.none)

        DropStop ->
            ( { model | dropping = False }, Cmd.none)

        NoOp ->
            ( model, Cmd.none )


maximumWithDefault : comparable -> List comparable -> comparable
maximumWithDefault default =
    Maybe.withDefault default << List.maximum


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


move : Int -> Block -> Block
move dx block =
    let
        maxX = playFieldSize.cols - (blockWidth block)
        newX = clamp 0 maxX <| block.x + dx
    in
        { block | x = newX }


rotate : Block -> Block
rotate block =
    let
        flattened = List.indexedMap (\y cellList -> List.map (\cell -> ((Tuple.first cell, y), Tuple.second cell)) cellList) (List.reverse block.grid)
                        |> List.concat

        foobar row ((x, y), c) =
            if row == x then
                Just (y, c)
            else
                Nothing

        newGrid = List.range 0 (blockWidth block - 1)
                    |> List.map (\row -> List.filterMap (foobar row) flattened)
    in
        { block | grid = newGrid }


updateActiveBlock : Model -> Time.Time -> Model
updateActiveBlock model time =
    if time < model.nextDrop then
        model
    else
        let
            block = model.activeBlock
            proposedBlock = { block | y = block.y + 1 }
            interval = if model.dropping then 50 else 200
            nextDrop = time + interval * Time.millisecond
        in
            if detectCollision proposedBlock model.landed then
                let
                    newActive = newBlock
                    landed = copyBlock block model.landed
                    (removed, newLanded) = removeFullRows landed

                in
                    if detectCollision newActive newLanded then
                        gameOver model
                    else
                        { model
                        | landed = newLanded
                        , activeBlock = newBlock
                        , nextDrop = nextDrop
                        , score = model.score + removed * 10
                        }
            else
                { model | activeBlock = proposedBlock, nextDrop = nextDrop}


gameOver model =
    { model | playing = False, gameOver = True}

padGrid : Int -> Grid -> Grid
padGrid nr grid =
    case nr of
        0 ->
            grid
        n ->
            padGrid (n - 1) <| [] :: grid


removeFullRows : Grid -> (Int, Grid)
removeFullRows grid =
    let
        newGrid = List.filter (\l -> List.length l /= playFieldSize.cols) grid
        removed = (List.length grid) - (List.length newGrid)
    in
        (removed, padGrid removed newGrid)


copyBlock : Block -> Grid -> Grid
copyBlock block grid =
    let
        movedBlockGrid = block.grid
            |> List.map (\l -> List.map (Tuple.mapFirst ((+) block.x)) l)

        a = List.take block.y grid
        fromY = List.drop block.y grid
        toChange = List.take (blockHeight block) fromY
        b = List.drop (blockHeight block) fromY

        new = List.map2 List.append movedBlockGrid toChange
    in
        List.concat [a, new, b]


detectCollision : Block -> Grid -> Bool
detectCollision block grid =
    let
        gridWithStop = List.append grid [fullRow]
        findCollisionsInRows blockRow landedRow =
            let
                blockXs = List.map Tuple.first blockRow |> List.map ((+) block.x)
                landedXs = List.map Tuple.first landedRow
            in
                List.map (flip List.member blockXs) landedXs
                    |> List.any Basics.identity
    in
        List.drop block.y gridWithStop
            |> List.map2 findCollisionsInRows block.grid
            |> List.any Basics.identity


getColor : Int -> Color
getColor shapeId =
    case shapeId of
        0 -> red
        1 -> orange
        2 -> lightPurple
        3 -> blue
        4 -> green
        5 -> brown
        6 -> purple
        _ -> white


renderLine : List Cell -> List Collage.Form -> List Collage.Form
renderLine line rendered =
    case line of
        [] ->
            rendered

        (pos, color) :: rest ->
            let
                renderedCell = Collage.rect (toFloat gridSize) (toFloat gridSize)
                                |> Collage.filled (getColor color)
                                |> Collage.move ((toFloat (pos * gridSize)), 0)
            in
                renderLine rest <| renderedCell :: rendered


renderLines : Float -> Float -> Grid -> List Collage.Form -> List Collage.Form
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
                renderLines xOffset (yOffset + (toFloat gridSize)) rest <| renderedLine :: rendered


renderGrid : Float -> Float -> Grid -> Collage.Form
renderGrid xOffset yOffset grid =
    renderLines xOffset yOffset grid []
        |> Collage.group


renderBlock : Block -> Collage.Form
renderBlock block =
    let
        xOffset = (toFloat (block.x * gridSize))
        yOffset = (toFloat (block.y * gridSize))
    in
        renderGrid xOffset yOffset block.grid


{--
  Simplify the rendering code by applying a transformation on all forms drawn.

  Since Collage in elm-graphics puts origin in the center with Y growing upwards and draws all
  forms anchored to the center as well, flip the Y axis and translate origin to be half a gridSize
  in from the top left.
--}
canvasTranslation : Transform.Transform
canvasTranslation =
    let
        startX = -(toFloat (playFieldDimensions.width - gridSize)) / 2
        startY = -(toFloat (playFieldDimensions.height - gridSize)) / 2
    in
        Transform.multiply (Transform.scaleY -1) (Transform.translation startX startY)


view : Model -> Html Msg
view model =
    let
        togglePlayStr = if model.playing then "Pause" else "Start"
        str = if model.gameOver then "GAME OVER with score: " else "Score: "
    in
        div [ ]
            [ text <| str ++ (toString model.score)
            , viewPlayField model
            , button [ onClick TogglePlay ] [ text togglePlayStr ]
            ]


handleDownKey : KeyCode -> Msg
handleDownKey code =
    case code of
        65 ->
            Left
        68 ->
            Right
        87 ->
            Rotate
        83 ->
            DropStart
        _ ->
            NoOp

handleUpKey : KeyCode -> Msg
handleUpKey code =
    case code of
        83 ->
            DropStop
        _ ->
            NoOp

viewPlayField : Model -> Html Msg
viewPlayField model =
    let
        forms = [ renderGrid 0 0 model.landed
                , renderBlock model.activeBlock
                ]
    in
        Collage.collage playFieldDimensions.width playFieldDimensions.height
            [ Collage.groupTransform canvasTranslation forms ]
            |> Element.color gray
            |> Element.toHtml


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Sub.batch
            [ AF.times Tick
            , KB.downs handleDownKey
            , KB.ups handleUpKey
            ]
    else
        Sub.none
