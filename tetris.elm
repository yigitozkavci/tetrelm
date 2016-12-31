import Html exposing (Html, input, div)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Element exposing (toHtml)
import Text
import Color exposing(red, blue)
import Grid
import Board exposing (Board, board)
import Location exposing (Location)
import Shape exposing (Shape, shapeToForm, ShapeType(..))
import Time exposing (Time, millisecond)
import Random
import Collage exposing
  ( Form
  , collage
  , group
  , move
  )

main =
  Html.program
    { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Model =
  { shapes : List Shape
  , pieceInterval : Int
  }


model : Model
model =
  { shapes =
      [ { x = 3, y = 4, shapeType = L }
      ]
  , pieceInterval = 1
  }


init =
  (model, Cmd.none)


type Msg = Tick Time | EmptyMsg | RandomXPos Int | RandomType Int Int


dropOnePixel : Shape -> Shape
dropOnePixel shape =
  { shape | y = shape.y + 1 }


-- decideCmdByTime : Model -> Cmd Msg
-- decideCmdByTime model =
--   if model.time > 10 then (SendNewPiece) else Cmd.none


decodeShapeType : Int -> ShapeType
decodeShapeType encodedType =
  case encodedType of
    1 -> L
    2 -> RL
    3 -> I
    4 -> S
    5 -> T
    _ -> L


sendNewPiece : Int -> Int -> Model -> Model
sendNewPiece xPos shapeType model =
  { model | shapes = { x = xPos, y = 0, shapeType = (decodeShapeType shapeType) } :: model.shapes }


update msg model =
  case msg of
    Tick time ->
      if model.pieceInterval >= 3 then
        (model, Random.generate RandomXPos (Random.int 1 (board.width//board.tileSize)))
      else
        ({ model | pieceInterval = model.pieceInterval + 1 , shapes = List.map dropOnePixel model.shapes }, Cmd.none)
    RandomXPos xPos ->
      (model, Random.generate (RandomType xPos) (Random.int 1 5))
    RandomType xPos shapeType ->
      (sendNewPiece xPos shapeType { model | pieceInterval = 1, shapes = List.map dropOnePixel model.shapes }, Cmd.none)
    EmptyMsg ->
      (model, Cmd.none)
      


tetris : Model -> List Form
tetris model =
  Grid.gridLines :: (List.map shapeToForm model.shapes)


tetrisHtml : Model -> Html Msg
tetrisHtml model =
  collage 301 601
    [ tetris model |> group |> move (-150, 300)
    ] |> toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (100 * millisecond) Tick


view model =
  div []
    [ tetrisHtml model
    ]
