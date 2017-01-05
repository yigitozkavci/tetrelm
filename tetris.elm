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
import Char exposing (toCode)
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
  , state = Playing
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
setRow : Int -> Matrix.Matrix Int -> List Int -> Matrix.Matrix Int
setRow row blockMap newRow =
  List.indexedMap (\i col ->
    List.indexedMap (\innerRow innerVal ->
      if innerRow == row then
        case (Array.get i (Array.fromList newRow)) of
          Just a -> a
          Nothing -> 0
      else innerVal
    ) col
  ) (Matrix.toList blockMap) |> Matrix.fromList


carryRowToBottom : BlockMap -> Int -> BlockMap
carryRowToBottom blockMap row =
  if row == 0 then
    blockMap
  else
    carryRowToBottom (setRow (row + 1) blockMap (getRow row blockMap)) (row - 1)


clearRowSeq : BlockMap -> Int -> BlockMap
clearRowSeq blockMap row =
  if row == 0 then
    blockMap
  else
    if List.all (\s -> s == 1) (getRow row blockMap) then
      carryRowToBottom blockMap (row - 1)
    else
      clearRowSeq blockMap (row - 1)

clearIfNecessary : BlockMap -> BlockMap
clearIfNecessary blockMap =
  clearRowSeq blockMap (board.height//board.tileSize)


dropShapesByOnePixel : Model -> (Model, Cmd Msg)
dropShapesByOnePixel model =
  let
    shapesAndBlockMap =
      List.map (\shape -> 
        if shape.isActive then
          if Shape.dropAllowed model.blockMap shape then
            ({ shape | y = shape.y + 1}, model.blockMap, True)
          else
            ({ shape | isActive = False }, feedShapeToBlockMap shape model.blockMap, False)
        else
          (shape, model.blockMap, True)
      ) model.shapes

    shapes = List.map (\(shape, _, _) -> shape) shapesAndBlockMap

    (_, blockMap, _) =
      case (shapesAndBlockMap |> List.head) of
        Just a ->
          a
        Nothing ->
          (emptyShape, model.blockMap, False)

    isStickedToGround = shapesAndBlockMap |> List.map (\(_, _, status) -> status) |> List.foldr (&&) True

    (state, cmd) =
      if isStickedToGround then
        let activeShape = onlyActive shapes in
          case activeShape of
            Just s ->
              if Shape.positionValid s then
                (model.state, Cmd.none)
              else
                (Over, Cmd.none)
            Nothing ->
              (model.state, Cmd.none)
      else
        (model.state, Random.generate RandomXPos (Random.int 0 (board.width//board.tileSize)))
  in
    ({ model | shapes = shapes, blockMap = (clearIfNecessary blockMap), state = state }, cmd)


onlyActive : List Shape -> Maybe Shape
onlyActive shapes =
  shapes |> List.filter (\s -> s.isActive) |> List.head


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


rotateActiveShape : Model -> Model
rotateActiveShape model =
  let
    shapes = model.shapes |> List.map (\s -> if s.isActive then Shape.rotateBy 1 s else s)
  in
    { model | shapes = shapes }
movePiece : Model -> Direction -> BlockMap -> Model
movePiece model direction blockMap =
  let
    shapes = model.shapes
      |> List.map (\s -> if s.isActive then Shape.movePiece s direction blockMap else s)
  in
    { model | shapes = shapes }


sendPieceKey =
  toCode 'S'
  
restartKey =
  toCode 'W'

leftKey =
  37

upKey =
  38

rightKey =
  39

downKey =
  40

spaceKey =
  32

enterKey =
  13


finish : Model -> (Model, Cmd Msg)
finish model =
  (model, Cmd.none)


dropActiveShape : Model -> (Model, Cmd Msg)
dropActiveShape model =
  let
    (newModel, _) = dropShapesByOnePixel model
    activeShape = onlyActive newModel.shapes
  in  
    case activeShape of
      Just shape ->
        dropActiveShape newModel
      Nothing ->
        (newModel, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      case model.state of
        Playing ->
          dropShapesByOnePixel model
        Over ->
          finish model
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
      if key == spaceKey then
        dropActiveShape model
      else if key == enterKey then
        (model, Random.generate RandomXPos (Random.int 0 (board.width//board.tileSize)))
      else if key == leftKey then
        (movePiece model Left model.blockMap, Cmd.none)
      else if key == rightKey then
        (movePiece model Right model.blockMap, Cmd.none)
      else if key == upKey then
        (rotateActiveShape model, Cmd.none)
      else if key == downKey then
        dropShapesByOnePixel model
      else
        (model, Cmd.none)
    EmptyMsg ->
      (model, Cmd.none)


tetris : Model -> List Form
tetris model =
  (Grid.gridLines model) :: (List.map shapeToForm (model.shapes |> List.filter (\s -> s.isActive)))


tetrisHtml : Model -> Html Msg
tetrisHtml model =
  collage 304 604
    [ tetris model |> group |> move (-150, 300)
    ] |> toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
  [ Time.every (500 * millisecond) Tick
  , Keyboard.ups KeyUp
  ] |> Sub.batch


view model =
  div []
    [ tetrisHtml model
    ]
