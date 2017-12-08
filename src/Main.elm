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


defaultTimeToUpdate : Time.Time
defaultTimeToUpdate = 400 * Time.millisecond


boostedTimeToUpdate : Time.Time
boostedTimeToUpdate = 50 * Time.millisecond


playFieldSize : { cols : Int, rows : Int}
playFieldSize = { cols = 10, rows = 20 }


playFieldDimensions : { width : Int, height : Int}
playFieldDimensions =
    { width = playFieldSize.cols * Grid.cellSize
    , height = playFieldSize.rows * Grid.cellSize
    }


type alias Score = Int


type alias Game =
    { grid : Grid.Grid
    , activeBlock : Block.Block
  --  , nextBlock : Block.Block
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
    | NextBlock Block.Block
    | NoOp -- Needed by handleKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.state) of
        (Tick diff, Playing game) ->
            let
                (updatedGame, cmd) = updateGame game diff
            in
                { model | state = Playing updatedGame } ! [ cmd ]

        (TogglePlay, _) ->
            togglePlaying model

        (Left, Playing game)->
            { model | state = Playing <| modifyActiveBlock (Block.moveOn -1) game } ! []

        (Right, Playing game) ->
            { model | state = Playing <| modifyActiveBlock (Block.moveOn 1) game } ! []

        (Rotate, Playing game) ->
            { model | state = Playing <| modifyActiveBlock Block.rotateOn game } ! []

        (Boost onOff, Playing game) ->
            { model | state = Playing <| setBoost onOff game } ! []

        (Reset, _) ->
            init

        (NextBlock block, _) ->
            newBlockSpawned model block ! []

        _ ->
            model ! []


spawnNewBlock : Cmd Msg
spawnNewBlock =
    Random.generate NextBlock Block.getRandom


newBlockSpawned : Model -> Block.Block -> Model
newBlockSpawned model block =
    case model.state of
        Playing game ->
            if Block.detectCollisionInGrid block game.grid then
                { model | state = GameOver game.score }
            else
                { model | state = Playing { game | activeBlock = block } }

        Initial ->
            startPlaying model block

        _ ->
            model


togglePlaying : Model -> (Model, Cmd Msg)
togglePlaying model =
    case model.state of
        Playing game ->
            { model | state = Paused game } ! []

        Paused game ->
            { model | state = Playing game } ! []

        Initial ->
            model ! [ spawnNewBlock ]

        GameOver _ ->
            { model | state = Initial } ! [ spawnNewBlock ]


startPlaying : Model -> Block.Block -> Model
startPlaying model block =
    let
        game = Game (Grid.empty playFieldSize.cols playFieldSize.rows) block 0 defaultTimeToUpdate False
    in
        { model | state = Playing game }


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


removeFullRows : Game -> Game
removeFullRows game =
    let
        (removed, grid) = Grid.removeFullRows game.grid
    in
        { game | grid = grid, score = game.score + 10 * removed }


landBlock : Game -> Game
landBlock =
    removeFullRows << copyBlockToGrid


resetTimeToNextUpdate : (Game, Cmd Msg) -> (Game, Cmd Msg)
resetTimeToNextUpdate (game, cmd) =
    let
        timeToUpdate = if game.boost then boostedTimeToUpdate else defaultTimeToUpdate
    in
        ( { game | timeToUpdate = timeToUpdate }, cmd )


updateGame : Game -> Time.Time -> (Game, Cmd Msg)
updateGame game diff =
    let
        t = game.timeToUpdate - diff
    in
        if t < 0 then
            resetTimeToNextUpdate <| advanceGame game
        else
            { game | timeToUpdate = t } ! []


advanceGame : Game -> (Game, Cmd Msg)
advanceGame game =
    case Block.moveYOn 1 game.grid game.activeBlock of
        Ok block ->
            { game | activeBlock = block } ! []

        _ ->
            landBlock game ! [ spawnNewBlock ]


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

        Playing game ->
            div [ ]
                [ text <| "Score: " ++ (toString game.score)
                , viewPlayField game.grid <| Just game.activeBlock
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
