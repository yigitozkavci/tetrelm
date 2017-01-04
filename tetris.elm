import Html exposing (Html, input, div)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Element exposing (toHtml)
import Text
import Color exposing(red, blue)
import Grid
import Types exposing (..)
import Location
import Matrix exposing (matrix)
import Shape exposing (shapeToForm)
import Time exposing (Time, millisecond)
import Random
import Keyboard exposing (KeyCode)
import Array exposing (Array)
import Collage exposing
  ( Form
  , collage
  , group
  , move
  )

main =
  Html.program
    { init = init, update = update, subscriptions = subscriptions, view = view }

initModel : Model
initModel =
  { shapes = []
  , pieceInterval = 1
  , blockMap = matrix (board.width//board.tileSize) (board.height//board.tileSize) (\_ -> 0)
  }


init =
  (initModel, Cmd.none)


emptyShape : Shape
emptyShape =
  Shape 0 0 I [(0, 0)] False


{- Gets the row of a matrix
-}
getRow : Int -> Matrix.Matrix Int -> List Int
getRow row blockMap =
  List.map (\col ->
    List.indexedMap (\innerRow val ->
      if innerRow == row then val else 0
    ) col |> List.foldr (+) 0
  ) (Matrix.toList blockMap)


{- Sets the row of a matrix
-}
setRow : Int -> Matrix.Matrix Int -> Int -> Matrix.Matrix Int
setRow row blockMap value =
  List.map (\col ->
    List.map (\innerRow ->
      if innerRow == row then value else 0
    ) col
  ) (Matrix.toList blockMap) |> Matrix.fromList


clearIfNecessary : BlockMap -> BlockMap
clearIfNecessary blockMap =
  let
      _ = Debug.log "wow" (getRow 19 blockMap)
  in
    blockMap


dropShapesByOnePixel : Model -> Model
dropShapesByOnePixel model =
  let
    shapesAndBlockMap =
      List.map (\shape -> 
        if shape.isActive then
          if Shape.dropAllowed model.blockMap shape then
            ({ shape | y = shape.y + 1}, model.blockMap)
          else
            ({ shape | isActive = False }, feedShapeToBlockMap shape model.blockMap)
        else
          (shape, model.blockMap)
      ) model.shapes

    shapes = List.map (\(shape, _) -> shape) shapesAndBlockMap

    (_, blockMap) =
      case (shapesAndBlockMap |> List.head) of
        Just a ->
          a
        Nothing ->
          (emptyShape, model.blockMap)
  in
    { model | shapes = shapes, blockMap = (clearIfNecessary blockMap) }


feedShapeToBlockMap : Shape -> BlockMap -> BlockMap
feedShapeToBlockMap shape blockMap =
  let
    newShape = Shape.withShiftedLocations shape
  in
    Matrix.mapWithLocation (\loc val -> if List.member loc newShape.blockLocations then 1 else val) blockMap


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
  , isActive = True
  } |> (Shape.rotateBy rotateAmount)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
        (dropShapesByOnePixel model, Cmd.none)
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
  (Grid.gridLines model) :: (List.map shapeToForm model.shapes)


tetrisHtml : Model -> Html Msg
tetrisHtml model =
  collage 304 604
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
