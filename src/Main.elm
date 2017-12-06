module Main exposing (main)

import Color exposing (..)
import Html exposing (..)
import Collage
import Transform exposing (..)
import Element
import Dict exposing (..)


gridSize : Int
gridSize = 25
playFieldSize = { cols = 10, rows = 20 }
playFieldDimensions =
    { width = playFieldSize.cols * gridSize
    , height = playFieldSize.rows * gridSize
    }

f c x = (x, c)

iBlock : Grid
iBlock =
    [ List.map (f 0) <| List.range 0 3 ]

jBlock =
    [ List.map (f 1) <| List.range 0 2
    , [ (2, 1) ]
    ]

lBlock =
    [ List.map (f 2) <| List.range 0 2
    , [ (0, 2) ]
    ]

oBlock =
    [ [ (0, 3), (1, 3) ]
    , [ (0, 3), (1, 3) ]
    ]

sBlock =
    [ [ (1, 4), (2, 4) ]
    , [ (0, 4), (1, 4) ]
    ]

tBlock =
    [ List.map (f 5) <| List.range 0 2
    , [ (1, 5) ]
    ]

zBlock =
    [ [ (0, 6), (1, 6) ]
    , [ (1, 6), (2, 6) ]
    ]


type Msg = NoOp

type alias Position = (Int, Int)

-- Once the blocks have landed, copy their location over to the PlayField
type alias Cell = (Int, Int)

type alias Grid = List (List Cell)

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
    { landed : Grid
    , dropping : (Position, Grid)
    }


init : (Model, Cmd Msg)
init =
    ( Model demoGrid <| ((3, 0), iBlock), Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


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


renderLines : Float -> Float -> Grid -> List Collage.Form ->  List Collage.Form
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
                renderLines xOffset (yOffset - (toFloat gridSize)) rest <| renderedLine :: rendered


renderGrid : Float -> Float -> Grid -> Collage.Form
renderGrid xOffset yOffset grid =
    renderLines xOffset yOffset grid []
        |> Collage.group

startX =
    -(toFloat (playFieldDimensions.width - gridSize)) / 2

startY =
    (toFloat (playFieldDimensions.height - gridSize)) / 2


view : Model -> Html Msg
view model =
    let
        ((x, y), g) = model.dropping

        _ = model.dropping |> Debug.log("Meh")

        xOffset = startX + (toFloat (x * gridSize))
        yOffset = startY - (toFloat (y * gridSize))
        forms = [ renderGrid startX startY model.landed
                , renderGrid xOffset yOffset g
                , Collage.rect 25 25 |> Collage.filled brown |> Collage.move (-100, startY)
                ]
    in
        Collage.collage playFieldDimensions.width playFieldDimensions.height forms
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
    Sub.none
