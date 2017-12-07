module Main exposing (main)

import Grid exposing (..)
import Block exposing (..)

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
import Random

cellSize : Int
cellSize = 25


playFieldSize : { cols : Int, rows : Int}
playFieldSize = { cols = 10, rows = 20 }


playFieldDimensions : { width : Int, height : Int}
playFieldDimensions =
    { width = playFieldSize.cols * cellSize
    , height = playFieldSize.rows * cellSize
    }


type alias Model =
    { playing : Bool
    , gameOver : Bool
    , landed : Grid
    , nextDrop : Time.Time
    , boost : Bool
    , score : Int
    , activeBlock : Block
    , seed : Random.Seed
    }


init : (Model, Cmd Msg)
init =
    let
        (seed, block) = getRandomBlock <| Random.initialSeed 0
        grid = emptyGrid playFieldSize.rows
    in
        ( Model False False grid 0 False 0 block seed, Cmd.none )


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
            Boost True
        _ ->
            NoOp

handleUpKey : KeyCode -> Msg
handleUpKey code =
    case code of
        83 ->
            Boost False
        _ ->
            NoOp


type Msg
    = Tick Time.Time
    | TogglePlay
    | Left
    | Right
    | Rotate
    | Boost Bool
    | NoOp -- Needed by handleKey


clampedMoveBlock : Int -> Block -> Block
clampedMoveBlock =
    Block.move 0 playFieldSize.cols


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ( updateActiveBlock model time, Cmd.none )

        TogglePlay ->
            ( { model | playing = not model.playing }, Cmd.none )

        Left ->
            ( { model | activeBlock = clampedMoveBlock -1 model.activeBlock }, Cmd.none )

        Right ->
            ( { model | activeBlock = clampedMoveBlock 1 model.activeBlock }, Cmd.none )

        Rotate ->
            ( { model | activeBlock = rotate model.activeBlock }, Cmd.none )

        Boost on ->
            ( { model | boost = on }, Cmd.none)

        NoOp ->
            ( model, Cmd.none )


updateActiveBlock : Model -> Time.Time -> Model
updateActiveBlock model time =
    if time < model.nextDrop then
        model
    else
        -- Needs refactoring
        let
            block = model.activeBlock
            proposedBlock = { block | y = block.y + 1 }
            interval = if model.boost then 50 else 200
            nextDrop = time + interval * Time.millisecond
        in
            if detectCollision proposedBlock.y (toGrid proposedBlock) model.landed then
                let
                    (seed, newActive) = getRandomBlock model.seed
                    landed = Grid.copyOnto block.y (toGrid block) model.landed
                    (removed, newLanded) = removeFullRows playFieldSize.cols landed

                in
                    if detectCollision newActive.y (toGrid newActive) newLanded then
                        gameOver model
                    else
                        { model
                        | landed = newLanded
                        , activeBlock = newActive
                        , nextDrop = nextDrop
                        , seed = seed
                        , score = model.score + removed * 10
                        }
            else
                { model | activeBlock = proposedBlock, nextDrop = nextDrop}


gameOver : Model -> Model
gameOver model =
    { model | playing = False, gameOver = True}


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
                renderLines xOffset (yOffset + (toFloat cellSize)) rest <| renderedLine :: rendered


renderGrid : Float -> Float -> Grid -> Collage.Form
renderGrid xOffset yOffset grid =
    renderLines xOffset yOffset grid []
        |> Collage.group


renderBlock : Block -> Collage.Form
renderBlock block =
    let
        xOffset = (toFloat (block.x * cellSize))
        yOffset = (toFloat (block.y * cellSize))
    in
        renderGrid xOffset yOffset block.grid


{--
  Simplify the rendering code by applying a transformation on all forms drawn.

  Since Collage in elm-graphics puts origin in the center with Y growing upwards and draws all
  forms anchored to the center as well, flip the Y axis and translate origin to be half a cellSize
  in from the top left.
--}
canvasTranslation : Transform.Transform
canvasTranslation =
    let
        startX = -(toFloat (playFieldDimensions.width - cellSize)) / 2
        startY = -(toFloat (playFieldDimensions.height - cellSize)) / 2
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


-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }
