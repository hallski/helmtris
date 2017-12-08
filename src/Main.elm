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
    , nextDrop : Time.Time
    , boost : Bool
    }


type GameState
    = Initial
    | Playing GameData
    | Paused GameData
    | GameOver Score


type alias Model =
    { state : GameState
    }


init : (Model, Cmd Msg)
init =
    Model Initial ! []


-- Update

type Msg
    = Tick Time.Time
    | TogglePlay
    | Left
    | Right
    | Rotate
    | Boost Bool
    | Reset
    | NextBlock Block.Block
    | NoOp -- Needed by handleKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            updateActiveBlock time model

        TogglePlay ->
            togglePlaying model

        Left ->
            modifyActiveBlock (Block.moveOn -1) model ! []

        Right ->
            modifyActiveBlock (Block.moveOn 1) model ! []

        Rotate ->
            modifyActiveBlock Block.rotateOn model ! []

        Boost onOff ->
            setBoost onOff model ! []

        Reset ->
            init

        NextBlock block ->
            newBlockSpawned model block ! []

        NoOp ->
            model ! []


spawnNewBlock : Cmd Msg
spawnNewBlock =
    Random.generate NextBlock Block.getRandom


newBlockSpawned : Model -> Block.Block -> Model
newBlockSpawned model block =
    case model.state of
        Playing data ->
            if Block.detectCollisionInGrid block data.grid then
                { model | state = GameOver data.score }
            else
                { model | state = Playing { data | activeBlock = block } }

        Initial ->
            startPlaying model block

        _ ->
            model


togglePlaying : Model -> (Model, Cmd Msg)
togglePlaying model =
    case model.state of
        Playing data ->
            { model | state = Paused data } ! []

        Paused data ->
            { model | state = Playing data } ! []

        Initial ->
            model ! [ spawnNewBlock ]

        GameOver _ ->
            { model | state = Initial } ! [ spawnNewBlock ]


startPlaying : Model -> Block.Block -> Model
startPlaying model block =
    let
        data = GameData (Grid.empty playFieldSize.cols playFieldSize.rows) block 0 0 False
    in
        { model | state = Playing data }


setBoost : Bool -> Model -> Model
setBoost onOff model =
    case model.state of
        Playing data ->
            { model | state = Playing { data | boost = onOff } }

        _ ->
            model


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


copyBlockToGrid : GameData -> GameData
copyBlockToGrid data =
    { data | grid = Block.copyOntoGrid data.activeBlock data.grid }


removeFullRows : GameData -> GameData
removeFullRows data =
    let
        (removed, grid) = Grid.removeFullRows data.grid
    in
        { data | grid = grid, score = data.score + 10 * removed }


landBlock : GameData -> GameData
landBlock data =
    copyBlockToGrid data
        |> removeFullRows


updateActiveBlock : Time.Time -> Model -> (Model, Cmd Msg)
updateActiveBlock time model =
    case model.state of
        Playing data ->
            if time < data.nextDrop then
                model ! []
            else
                case Block.moveYOn 1 data.grid data.activeBlock of
                    Ok block ->
                        let
                            interval = if data.boost then 50 else 400
                            nextDrop = time + interval * Time.millisecond
                            newData = { data | activeBlock = block, nextDrop = nextDrop }
                        in
                            { model | state = Playing newData } ! []

                    _ ->
                        { model | state = Playing (landBlock data) } ! [ spawnNewBlock ]

        _ ->
            model ! []


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
                , viewPlayField (Grid.empty playFieldSize.cols playFieldSize.rows) Nothing
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
                , viewPlayField (Grid.empty playFieldSize.cols playFieldSize.rows) Nothing
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
