import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Element exposing (toHtml)
import Collage exposing (..)
import Text
import Color exposing(red, blue)
import Grid
import Board exposing (Board, board)
import Location exposing (Location)


main =
  Html.beginnerProgram { model = board, view = view, update = update }


type Msg = Increment


update msg model =
  model


type alias Block = Form


scaleLocation : Int -> Location -> Location
scaleLocation scale location =
  let (x, y) = location in
    ( scale//2 + x * scale
    , -scale//2 - y * scale
    )


generateBlock : Location -> Block
generateBlock location =
  filled red (square (toFloat board.tileSize))
    |> move (Grid.toFloatPos (scaleLocation board.tileSize location))


type Shape = L | RL | I | S


shiftLocationBy : Location -> Location -> Location
shiftLocationBy startingLocation location =
  let (startX, startY) = startingLocation in
  let (x, y) = location in
  (startX + x, startY + y)


blockShapeLocations : Location -> Shape -> List Location
blockShapeLocations startingLocation shape =
  case shape of
    L ->
      List.map (shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (2, 0)
        , (0, 1)
        ]
    RL ->
      List.map (shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (2, 0)
        , (2, 1)
        ]
    I ->
      List.map (shiftLocationBy startingLocation)
        [ (0, 0)
        , (0, 1)
        , (0, 2)
        , (0, 3)
        ]
    S ->
      List.map (shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (0, 1)
        , (1, 1)
        ]


generateShape : Location -> Shape -> Form
generateShape location shape =
  List.map (\location -> generateBlock location)
    (blockShapeLocations location shape) |> group

tetris : List Form
tetris =
  [ generateShape (2, 4) L
  , generateShape (1, 7) S
  , Grid.gridLines
  ]


tetrisHtml : Html Msg
tetrisHtml =
  collage 301 601
    [ tetris |> group |> move (-150, 300)
    ] |> toHtml


view model =
  div []
    [ tetrisHtml
    ]
