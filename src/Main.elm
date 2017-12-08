module Main exposing (main)

import Grid
import Block

import AnimationFrame as AF
import Char exposing (KeyCode)
import Collage
import Color exposing (..)
import Element
import Html exposing (..)
import Html.Events exposing (onClick, on, keyCode)
import Keyboard as KB
import Random
import Time
import Transform exposing (..)


playFieldSize : { cols : Int, rows : Int}
playFieldSize = { cols = 10, rows = 20 }


playFieldDimensions : { width : Int, height : Int}
playFieldDimensions =
    { width = playFieldSize.cols * Grid.cellSize
    , height = playFieldSize.rows * Grid.cellSize
    }

type alias Score = Int

type alias GameData =
    { grid : Grid.Grid
    , activeBlock : Block.Block
  --  , nextBlock : Block.Block
    , score : Score
    }


type GameState
    = Initial
    | Playing GameData
    | Paused GameData
    | GameOver Score


type alias Model =
    { state : GameState
    , nextDrop : Time.Time
    , boost : Bool -- -> Block.Block || GameData
    , seed : Random.Seed
    }


init : (Model, Cmd Msg)
init =
    ( Model Initial 0 False <| Random.initialSeed 0
    , Random.generate SetSeed (Random.int Random.minInt Random.maxInt))


-- Update

type Msg
    = Tick Time.Time
    | TogglePlay
    | Left
    | Right
    | Rotate
    | Boost Bool
    | Reset
    | SetSeed Int
    | NoOp -- Needed by handleKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            model
                |> updateActiveBlock time
                |> noCmd

        TogglePlay ->
            model
                |> togglePlaying
                |> noCmd

        Left ->
            model
                |> modifyActiveBlock (Block.moveOn -1)
                |> noCmd

        Right ->
            model
                |> modifyActiveBlock (Block.moveOn 1)
                |> noCmd

        Rotate ->
            model
                |> modifyActiveBlock Block.rotateOn
                |> noCmd

        Boost onOff ->
            model
                |> setBoost onOff
                |> noCmd

        Reset ->
            init

        SetSeed randomInt ->
            model
                |> reseed randomInt
                |> noCmd

        NoOp ->
            model |> noCmd


togglePlaying : Model -> Model
togglePlaying model =
    case model.state of
        Playing data ->
            { model | state = Paused data }

        Paused data ->
            { model | state = Playing data }

        Initial ->
            startPlaying model

        GameOver _ ->
            startPlaying model

startPlaying : Model -> Model
startPlaying model =
    let
        (seed, block) = Block.getRandom model.seed
        data = GameData (Grid.empty playFieldSize.cols playFieldSize.rows) block 0
    in
        { model | state = Playing data, seed = seed, nextDrop = 0 }

setBoost : Bool -> Model -> Model
setBoost onOff model =
    { model | boost = onOff }


reseed : Int -> Model -> Model
reseed newSeed model =
    case model.state of
        Initial ->
            { model | seed = Random.initialSeed newSeed }

        _ ->
            model


noCmd : Model -> (Model, Cmd Msg)
noCmd model =
    ( model, Cmd.none )


modifyActiveBlock : Block.BlockManipulation -> Model -> Model
modifyActiveBlock fn model =
    case model.state of
        Playing data ->
            let
                newData = case fn data.grid data.activeBlock of
                            Ok block ->
                                { data | activeBlock = block }

                            _ ->
                                data
            in
                { model | state = Playing newData }
        _ ->
            model


updateActiveBlock : Time.Time -> Model -> Model
updateActiveBlock time model =
    case model.state of
        Playing data ->
            if time < model.nextDrop then
                model
            else
                -- Needs refactoring
                let
                    block = data.activeBlock
                    proposedBlock = Block.moveYOn 1 data.grid block
                    interval = if model.boost then 50 else 400
                    nextDrop = time + interval * Time.millisecond
                in
                    case proposedBlock of
                        Ok newBlock ->
                            let
                                newData = { data | activeBlock = newBlock }
                            in

                            { model | state = Playing newData, nextDrop = nextDrop }
                        _ ->
                            let
                                (seed, newActive) = Block.getRandom model.seed
                                landed = Block.copyOntoGrid block data.grid
                                (removed, newGrid) = Grid.removeFullRows landed
                            in
                                if Block.detectCollisionInGrid newActive newGrid then
                                    gameOver model data.score
                                else
                                    let
                                        newData = { data
                                                  | grid = newGrid
                                                  , activeBlock = newActive
                                                  , score = data.score + removed * 10
                                                  }
                                    in
                                        { model
                                        | state = Playing newData
                                        , seed = seed
                                        , nextDrop = nextDrop
                                        }
        _ ->
            model

gameOver : Model -> Int -> Model
gameOver model score =
    { model | state = GameOver score }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing _ ->
            Sub.batch
                [ AF.times Tick
                , KB.downs handleDownKey
                , KB.ups handleUpKey
                ]

        _ ->
            Sub.none

handleDownKey : KeyCode -> Msg
handleDownKey code =
    case code of
        -- WASD
        65 -> Left
        68 -> Right
        87 -> Rotate
        83 -> Boost True

        -- Arrow keys
        37 -> Left
        38 -> Rotate
        39 -> Right
        40 -> Boost True

        _ -> NoOp

handleUpKey : KeyCode -> Msg
handleUpKey code =
    case code of
        -- WASD
        83 -> Boost False

        -- Arrow keys
        40 -> Boost False

        _ -> NoOp


-- View

{--
  Simplify the rendering code by applying a transformation on all forms drawn.

  Since Collage in elm-graphics puts origin in the center with Y growing upwards and draws all
  forms anchored to the center as well, flip the Y axis and translate origin to be half a cellSize
  in from the top left.
--}
canvasTranslation : Transform.Transform
canvasTranslation =
    let
        startX = -(toFloat (playFieldDimensions.width - Grid.cellSize)) / 2
        startY = -(toFloat (playFieldDimensions.height - Grid.cellSize)) / 2
    in
        Transform.multiply (Transform.scaleY -1) (Transform.translation startX startY)


view : Model -> Html Msg
view model =
    case model.state of
        GameOver score ->
            div [ ]
                [ text <| "GAME OVER with score: " ++ (toString score)
                , viewPlayField (Grid.empty playFieldSize.cols playFieldSize.rows) <| Nothing
                , button [ onClick Reset ] [ text "Reset" ]
                ]

        Playing data ->
            div [ ]
                [ text <| "Score: " ++ (toString data.score)
                , viewPlayField data.grid <| Just data.activeBlock
                , button [ onClick TogglePlay ] [ text "Pause" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]

        Paused data ->
            div [ ]
                [ text <| "Score: " ++ (toString data.score)
                , viewPlayField data.grid <| Nothing
                , button [ onClick TogglePlay ] [ text "Play" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]

        Initial ->
            div [ ]
                [ text <| "Press Play to start playing"
                , viewPlayField (Grid.empty playFieldSize.cols playFieldSize.rows) <| Nothing
                , button [ onClick TogglePlay ] [ text "Play" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]


viewPlayField : Grid.Grid -> Maybe Block.Block -> Html Msg
viewPlayField grid maybeBlock =
    let
        forms = case maybeBlock of
            Just block ->
                [ Grid.render 0 0 grid
                , Block.render block
                ]

            Nothing ->
                [ Grid.render 0 0 grid ]

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
