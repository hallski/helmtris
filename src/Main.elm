module Main exposing (main)

import Grid
import Block

import AnimationFrame as AF
import Char exposing (KeyCode)
import Collage
import Color exposing (..)
import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, keyCode)
import Keyboard as KB
import Random
import Time
import Transform exposing (..)


defaultTimeToUpdate : Time.Time
defaultTimeToUpdate = 400 * Time.millisecond


boostedTimeToUpdate : Time.Time
boostedTimeToUpdate = 50 * Time.millisecond


playFieldSize : { cols : Int, rows : Int}
playFieldSize = { cols = 10, rows = 20 }


type alias Score = Int


type alias Game =
    { grid : Grid.Grid
    , activeBlock : Block.Block
    , nextBlock : Maybe Block.Block
    , score : Score
    , timeToUpdate : Time.Time
    , boost : Bool
    }


type GameState
    = Initial
    | Playing Game
    | Paused Game
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
    | InitialBlocks (Block.Block, Block.Block)
    | NextBlock Block.Block
    | NoOp -- Needed by handleKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.state) of
        (Tick diff, Playing game) ->
            let
                (updatedGameState, cmd) = updateGame game diff
            in
                { model | state = updatedGameState } ! [ cmd ]

        (TogglePlay, _) ->
            togglePlaying model

        (Left, Playing game)->
            playingNoCmd model <| modifyActiveBlock (Block.moveOn -1) game

        (Right, Playing game) ->
            playingNoCmd model <| modifyActiveBlock (Block.moveOn 1) game

        (Rotate, Playing game) ->
            playingNoCmd model <| modifyActiveBlock Block.rotateOn game

        (Boost onOff, Playing game) ->
            playingNoCmd model <| setBoost onOff game

        (Reset, _) ->
            init

        (InitialBlocks blocks, Initial) ->
            startPlaying model blocks ! []

        (NextBlock block, Playing game) ->
            playingNoCmd model <| nextBlockSpawned game block

        _ ->
            model ! []


playingNoCmd : Model -> Game -> (Model, Cmd Msg)
playingNoCmd model game =
    { model | state = Playing game } ! []


spawnNextBlock : Cmd Msg
spawnNextBlock =
    Random.generate NextBlock Block.getRandom

spawnInitialBlocks : Cmd Msg
spawnInitialBlocks =
    Random.generate InitialBlocks <| Random.pair Block.getRandom Block.getRandom


togglePlaying : Model -> (Model, Cmd Msg)
togglePlaying model =
    case model.state of
        Playing game ->
            { model | state = Paused game } ! []

        Paused game ->
            { model | state = Playing game } ! []

        Initial ->
            model ! [ spawnInitialBlocks ]

        GameOver _ ->
            { model | state = Initial } ! [ spawnInitialBlocks ]


startPlaying : Model -> (Block.Block, Block.Block) -> Model
startPlaying model (block, nextBlock) =
    let
        grid = Grid.empty playFieldSize.cols playFieldSize.rows
        game = Game grid block (Just nextBlock) 0 defaultTimeToUpdate False
    in
        { model | state = Playing game }


nextBlockSpawned : Game -> Block.Block -> Game
nextBlockSpawned game block =
    { game | nextBlock = Just block }


setBoost : Bool -> Game -> Game
setBoost onOff game =
    { game | boost = onOff }


modifyActiveBlock : Block.BlockManipulation -> Game -> Game
modifyActiveBlock fn game =
    case fn game.grid game.activeBlock of
        Ok block ->
            { game | activeBlock = block }

        _ ->
            game


copyBlockToGrid : Game -> Game
copyBlockToGrid game =
    { game | grid = Block.copyOntoGrid game.activeBlock game.grid }


calculateScore : Int -> Int -> Int
calculateScore removedLines oldScore =
    case removedLines of
        0 ->
            oldScore

        n ->
            calculateScore (n - 1) (oldScore + n * 10)


removeFullRows : Game -> Game
removeFullRows game =
    let
        (removed, grid) = Grid.removeFullRows game.grid
    in
        { game | grid = grid, score = calculateScore removed game.score }


attemptNextBlock : Game -> (GameState, Cmd Msg)
attemptNextBlock game =
    case game.nextBlock of
        Just nextBlock ->
            if Block.detectCollisionInGrid nextBlock game.grid then
                GameOver game.score ! []
            else
                Playing { game | activeBlock = nextBlock, nextBlock = Nothing } ! [ spawnNextBlock ]

        Nothing ->
            Playing game ! []


landBlock : Game -> (GameState, Cmd Msg)
landBlock =
    attemptNextBlock << removeFullRows << copyBlockToGrid


resetTimeToNextUpdate : Game -> Game
resetTimeToNextUpdate game =
    let
        timeToUpdate = if game.boost then boostedTimeToUpdate else defaultTimeToUpdate
    in
        { game | timeToUpdate = timeToUpdate }


updateGame : Game -> Time.Time -> (GameState, Cmd Msg)
updateGame game diff =
    let
        timeToUpdate = game.timeToUpdate - diff
    in
        if timeToUpdate < 0 then
            advanceGame <| resetTimeToNextUpdate game
        else
            Playing { game | timeToUpdate = timeToUpdate } ! []


advanceGame : Game -> (GameState, Cmd Msg)
advanceGame game =
    case Block.moveYOn 1 game.grid game.activeBlock of
        Ok block ->
            Playing { game | activeBlock = block } ! []

        _ ->
            landBlock game


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing _ ->
            Sub.batch
                [ AF.diffs Tick
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
canvasTranslation : Int -> Int -> Transform.Transform
canvasTranslation width height =
    let
        startX = -(toFloat <| width - Grid.cellSize) / 2
        startY = -(toFloat <| height - Grid.cellSize) / 2
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

        Playing game ->
            div [ ]
                [ text <| "Score: " ++ (toString game.score)
                , div [ class "game-container" ]
                      [ viewPlayField game.grid <| Just game.activeBlock
                      , viewPreview game.nextBlock
                      ]
                , button [ onClick TogglePlay ] [ text "Pause" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]

        Paused game ->
            div [ ]
                [ text <| "Score: " ++ (toString game.score)
                , viewPlayField game.grid <| Nothing
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


viewPreview : Maybe Block.Block -> Html Msg
viewPreview maybeBlock =
    case maybeBlock of
        Just block ->
            let
                (width, height) = Block.dimensions block
                transformation = canvasTranslation width height
                forms = [ Block.renderPreview block ]

                preview = Collage.collage width height
                            [ Collage.groupTransform transformation forms ]
                            |> Element.toHtml
            in
                div [ class "preview" ]
                    [ h2 [] [ text "Next block:" ]
                    , preview
                    ]

        Nothing ->
            div [] []


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

        (gridWidth, gridHeight) = Grid.dimensions grid
        transformation = canvasTranslation gridWidth gridHeight
    in
        Collage.collage gridWidth gridHeight
            [ Collage.groupTransform transformation forms ]
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
