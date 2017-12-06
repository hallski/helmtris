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

emptyGrid : Grid
emptyGrid =
    List.repeat playFieldSize.rows []


demoGrid : Grid
demoGrid =
    let
        grid = List.append (List.take 18 emptyGrid)
                [ [(1, 3), (8, 4)] , [(1, 3), (2, 3), (6, 6), (0, 5)] ]
    in
        grid


-- For testing
newBlock : Block
newBlock =
    Block 1 0 jBlock


type alias Model =
    { playing : Bool
    , landed : Grid
    , nextDrop : Time.Time
    , activeBlock : Block
    }


init : (Model, Cmd Msg)
init =
    ( Model False demoGrid 0 <| Block 3 0 iBlock, Cmd.none )


type Msg
    = Tick Time.Time
    | TogglePlay
    | Left
    | Right
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
        maximumWithDefault 0 <| List.map rowWidth grid


move : Int -> Block -> Block
move dx block =
    let
        maxX = playFieldSize.cols - (blockWidth block) - 1
        newX = clamp 0 maxX <| block.x + dx
    in
        { block | x = newX }


updateActiveBlock : Model -> Time.Time -> Model
updateActiveBlock model time =
    if time < model.nextDrop then
        model
    else
        let
            block = model.activeBlock
            proposedBlock = { block | y = block.y + 1 }
            nextDrop = time + 200 * Time.millisecond
        in
            if detectCollision proposedBlock model.landed then
                { model
                | landed = copyBlock block model.landed
                , activeBlock = newBlock
                , nextDrop = nextDrop
                }
            else
                { model | activeBlock = proposedBlock, nextDrop = nextDrop}




copyBlock : Block -> Grid -> Grid
copyBlock block grid =
    -- Copy the block.grid onto grid
    grid


detectCollision : Block -> Grid -> Bool
detectCollision block grid =
    let
        findCollisionsInRows blockRow landedRow =
            let
                blockXs = List.map Tuple.first blockRow |> List.map ((+) block.x)
                landedXs = List.map Tuple.first landedRow
            in
                List.map (flip List.member blockXs) landedXs
                    |> List.any Basics.identity
    in
        List.drop block.y grid
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
    in
        div [ ]
            [ viewPlayField model
            , button [ onClick TogglePlay ] [ text togglePlayStr ]
            ]


handleKey : KeyCode -> Msg
handleKey code =
    case code of
        65 ->
            Left
        68 ->
            Right
        _ ->
            NoOp


viewPlayField : Model -> Html Msg
viewPlayField model =
    let
        forms = [ renderGrid 0 0 model.landed
                , renderBlock model.activeBlock
                , Collage.rect 25 25 |> Collage.filled darkBlue |> Collage.move (0, 0)
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
            , KB.downs handleKey
            ]
    else
        Sub.none
