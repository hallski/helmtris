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
    [ List.map (flip (,) <| 0) <| List.range 0 2
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


type alias Position = (Int, Int)

-- Once the blocks have landed, copy their location over to the PlayField
type alias Cell = (Int, Int)

type alias Grid = List (List Cell)
type alias Block = (Position, Grid)

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


type alias Model =
    { playing : Bool
    , landed : Grid
    , nextDrop : Time.Time
    , dropping : Block
    }


init : (Model, Cmd Msg)
init =
    ( Model False demoGrid 0 <| ((3, 0), iBlock), Cmd.none )


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
            ( updateDropping model time, Cmd.none )

        TogglePlay ->
            ( { model | playing = not model.playing }, Cmd.none )

        Left ->
            ( { model | dropping = moveDropping -1 model.dropping }, Cmd.none )

        Right ->
            ( { model | dropping = moveDropping 1 model.dropping }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


moveDropping : Int -> Block -> Block
moveDropping dx ((x, y), g) =
    let
        maxInRow l =
            l
            |> List.map Tuple.first
            |> List.maximum
            |> Maybe.withDefault 0

        maxX = List.map maxInRow g |> List.maximum |> Maybe.withDefault 0
        newX = clamp 0 (playFieldSize.cols - maxX - 1) <| x + dx
    in
        ((newX, y), g)


updateDropping : Model -> Time.Time -> Model
updateDropping model time =
    if time < model.nextDrop then
        model
    else
        let
            ((x, y), g) = model.dropping
            newBlock = ((x, y + 1), g)
            -- Check collision
        in
            { model | dropping = newBlock, nextDrop = time + Time.second }


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
        ((x, y), g) = model.dropping

        xOffset = (toFloat (x * gridSize))
        yOffset = (toFloat (y * gridSize))
        forms = [ renderGrid 0 0 model.landed
                , renderGrid xOffset yOffset g
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
