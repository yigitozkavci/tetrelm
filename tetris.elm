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
  { shapes = []
  , pieceInterval = 1
  }


init =
  (model, Cmd.none)


type Msg = Tick Time | EmptyMsg | RandomXPos Int | RandomType Int Int | RandomRotate Int ShapeType Int


dropOnePixel : Shape -> Shape
dropOnePixel shape =
  { shape | y = shape.y + 1 }


decodeShapeType : Int -> ShapeType
decodeShapeType encodedType =
  case encodedType of
    1 -> L
    2 -> RL
    3 -> I
    4 -> S
    5 -> T
    _ -> L

rotate : Location -> Location
rotate location =
  let (x, y) = location in
      (-y, x)

rotateBy : Int -> Shape -> Shape
rotateBy rotateAmount shape =
  if rotateAmount == 0 then
    shape
  else
    rotateBy (rotateAmount - 1) { shape | blockLocations = (List.map rotate shape.blockLocations) }

sendNewPiece : Int -> ShapeType -> Int -> Model -> Model
sendNewPiece xPos shapeType rotateAmount model =
  { model
  | pieceInterval = 1
  , shapes =
      ({ x = xPos, y = 0
      , shapeType = shapeType
      , blockLocations = Shape.initialBlockLocationsFor shapeType
      } |> (rotateBy rotateAmount)) :: model.shapes
  }

positionAllowed : Int -> ShapeType -> Bool
positionAllowed xPos shapeType =
  True

update msg model =
  case msg of
    Tick time ->
      let
        cmd =
          if model.pieceInterval >= 6 then
            Random.generate RandomXPos (Random.int 1 (board.width//board.tileSize))
          else
            Cmd.none
      in
        ( { model
          | pieceInterval = model.pieceInterval + 1
          , shapes = List.map dropOnePixel model.shapes
          }
        , cmd)
    RandomXPos xPos ->
      (model, Random.generate (RandomType xPos) (Random.int 1 5))
    RandomType xPos encodedShapeType ->
      let
        shapeType = decodeShapeType encodedShapeType
      in
        (model, Random.generate (RandomRotate xPos shapeType) (Random.int 0 3))
    RandomRotate xPos shapeType rotateAmount ->
      (sendNewPiece xPos shapeType rotateAmount model, Cmd.none)
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
