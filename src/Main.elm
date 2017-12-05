module Main exposing (main)

import Color exposing (..)
import Html exposing (..)
import Collage
import Transform exposing (..)
import Element
import Dict exposing (..)


gridSize = 30
playFieldDimensions = { width = 10, height = 20 }

{-- Blocks
  I [ [ 1 1 1 1 ] ]

  J [ [ 2, 2, 2 ]
    , [ 0, 0, 2 ]
    ]

  L [ [ 3, 3, 3 ]
    , [ 3, 0, 0 ]
    ]

  O [ [ 4, 4 ]
    . [ 4. 4 ]
    ]

  S [ [ 0, 5, 5 ]
    , [ 5, 5, 0 ]
    ]

  Z [ [ 6, 6, 0 ]
    , [ 0, 6, 6 ]
    ]

  T [ [ 7, 7, 7 ]
    , [ 0, 7, 0 ]
    ]
--}

type Msg = NoOp

type alias Position = (Int, Int)

-- Once the blocks have landed, copy their location over to the PlayField
type alias PlayField = Dict Position Color

type alias Cell = (Int, Int)

type alias Grid = List (List Cell)

emptyGrid : Grid
emptyGrid =
    List.repeat playFieldDimensions.height []


demoGrid : Grid
demoGrid =
    let
        grid = [(1, 3), (2, 3), (6, 6), (0, 5)] :: [(1, 3), (8, 4)] :: List.take 18 emptyGrid
    in
        grid

type alias Model =
    { landed : PlayField
    }

init =
    ( Model Dict.empty, Cmd.none )

update msg model =
    ( model, Cmd.none )

view model =
    viewPlayField model.landed

getColor : Int -> Color
getColor shapeId =
    case shapeId of
        1 -> red
        2 -> purple
        3 -> yellow
        4 -> orange
        5 -> lightPurple
        6 -> red
        _ -> gray

renderLine : List Cell -> List Collage.Form -> List Collage.Form
renderLine line rendered =
    case line of
        [] ->
            rendered

        (pos, color) :: rest ->
            let
                renderedCell = Collage.rect gridSize gridSize
                                |> Collage.filled (getColor color)
                                |> Collage.move ((toFloat pos) * gridSize, 0)
            in
                renderLine rest <| renderedCell :: rendered


renderLines : Grid -> Float -> List Collage.Form ->  List Collage.Form
renderLines lines yOffset rendered =
    case lines of
        [] ->
            rendered

        line :: rest ->
            let
                renderedLine =
                    renderLine line []
                        |> Collage.groupTransform (Transform.translation -135 yOffset)
            in
                renderLines rest (yOffset + gridSize) <| renderedLine :: rendered


renderGrid : Grid -> List Collage.Form
renderGrid grid =
    renderLines grid -285 []

viewPlayField : PlayField -> Html Msg
viewPlayField playField =
    let
        rendered = renderGrid demoGrid
                    |> Debug.log("Rendered")
    in

    Collage.collage (playFieldDimensions.width * gridSize) (playFieldDimensions.height * gridSize) rendered
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
