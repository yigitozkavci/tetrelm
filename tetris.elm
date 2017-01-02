import Html exposing (Html, input, div)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Element exposing (toHtml)
import Text
import Color exposing(red, blue)
import Grid
import Board exposing (Board, board)
import Location
import Matrix exposing (matrix)
import Shape exposing (Shape, shapeToForm, ShapeType(..))
import Time exposing (Time, millisecond)
import Random
import Keyboard exposing (KeyCode)
import Block exposing (BlockMap)
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
  , blockMap : BlockMap
  }


model : Model
model =
  { shapes = []
  , pieceInterval = 1
  , blockMap = matrix (board.width//board.tileSize) (board.height//board.tileSize) (\_ -> 0)
  }


init =
  (model, Cmd.none)


type Msg =
  Tick Time
  | EmptyMsg
  | RandomXPos Int
  | RandomType Int Int
  | RandomRotate Int ShapeType Int
  | KeyUp KeyCode


dropOnePixel : BlockMap -> Shape -> Shape
dropOnePixel blockMap shape =
  if Shape.dropAllowed blockMap shape then
    { shape | y = shape.y + 1 }
  else
    shape


decodeShapeType : Int -> ShapeType
decodeShapeType encodedType =
  case encodedType of
    1 -> L
    2 -> RL
    3 -> I
    4 -> S
    5 -> T
    _ -> L


generateNewPiece : Int -> ShapeType -> Int -> Model -> Shape
generateNewPiece xPos shapeType rotateAmount model =
  { x = xPos, y = 0
  , shapeType = shapeType
  , blockLocations = Shape.initialBlockLocationsFor shapeType
  } |> (Shape.rotateBy rotateAmount)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
        ({ model | shapes = List.map (dropOnePixel model.blockMap) model.shapes }, Cmd.none)
    RandomXPos xPos ->
      (model, Random.generate (RandomType xPos) (Random.int 1 5))
    RandomType xPos encodedShapeType ->
      let
        shapeType = decodeShapeType encodedShapeType
      in
        (model, Random.generate (RandomRotate xPos shapeType) (Random.int 0 3))
    RandomRotate xPos shapeType rotateAmount ->
      let newShape = generateNewPiece xPos shapeType rotateAmount model in
      if Shape.isXPosAllowed xPos newShape then
        ({ model | pieceInterval = 1, shapes = (newShape :: model.shapes) }, Cmd.none )
      else
        let cmd = Random.generate RandomXPos (Random.int 1 (board.width//board.tileSize)) in
          (model, cmd)
    KeyUp key ->
      (model, Random.generate RandomXPos (Random.int 0 (board.width//board.tileSize)))
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
  [ Time.every (100 * millisecond) Tick
  , Keyboard.ups KeyUp
  ] |> Sub.batch


view model =
  div []
    [ tetrisHtml model
    ]
