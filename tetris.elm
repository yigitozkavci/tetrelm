import Html exposing (Html, input, div, button, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
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
import Rest exposing (..)

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
  , blockMap = matrix (board.width//board.tileSize) (board.height//board.tileSize) (\_ -> 0)
  , state = NotStarted
  , gameId = Nothing
  }


init =
  (initModel, Cmd.none)


emptyShape : Shape
emptyShape =
  Shape 0 0 I [(0, 0)] False False


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
            ({ shape | y = shape.y + 1}, model.blockMap, False)
          else
            ({ shape | isActive = False, lastDropped = True }, feedShapeToBlockMap shape model.blockMap, True)
        else
          ({ shape | lastDropped = False }, model.blockMap, False)
      ) model.shapes

    shapes = List.map (\(shape, _, _) -> shape) shapesAndBlockMap

    (_, blockMap, _) =
      case (shapesAndBlockMap |> List.head) of
        Just a ->
          a
        Nothing ->
          (emptyShape, model.blockMap, False)

    isStickedToGround = shapesAndBlockMap |> List.map (\(_, _, status) -> status) |> List.foldr (||) False

    (state, cmd) =
      if isStickedToGround then
        let
          lastDroppedShape = List.filter (\s -> s.lastDropped) shapes |> List.head in
            case lastDroppedShape of
              Just s ->
                if Shape.positionValid s then
                  (model.state, generateNewPieceCmd)
                else
                  (Over, Cmd.none)
              Nothing ->
                (model.state, generateNewPieceCmd)
      else
        (model.state, Cmd.none)
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


generateNewPiece : Int -> ShapeType -> Int -> Model -> Shape
generateNewPiece xPos shapeType rotateAmount model =
  { x = xPos, y = 0
  , shapeType = shapeType
  , blockLocations = Shape.initialBlockLocationsFor shapeType
  , isActive = True
  , lastDropped = False
  } |> (Shape.rotateBy rotateAmount)


rotateActiveShape : Model -> Model
rotateActiveShape model =
  let
    shapes =
      model.shapes
        |> List.map (\shape ->
             if shape.isActive && Shape.rotateAllowed shape model.blockMap then
               Shape.rotateBy 1 shape
             else
               shape
           )
  in
    { model | shapes = shapes }


movePiece : Model -> Direction -> BlockMap -> Model
movePiece model direction blockMap =
  let
    shapes = model.shapes
      |> List.map (\s -> if s.isActive then Shape.movePiece s direction blockMap else s)
  in
    { model | shapes = shapes }


getKeyCode : KeyType -> KeyCode
getKeyCode key =
  case key of
    SendPieceKey ->
      toCode 'S'
    RestartKey ->
      toCode 'W'
    Left ->
      37
    Up ->
      38
    Right ->
      39
    Down ->
      40
    Space ->
      32
    Enter ->
      13
  
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
  -- ({ model | shapes = [], blockMap = (Matrix.matrix (board.width//board.tileSize) (board.height//board.tileSize) (\_ -> 0)) }, Cmd.none)
  ({ model | shapes = [] }, Cmd.none)


dropActiveShape : Model -> (Model, Cmd Msg)
dropActiveShape model =
  let
    (newModel, cmd) = dropShapesByOnePixel model
    activeShape = onlyActive newModel.shapes
  in  
    case activeShape of
      Just shape ->
        dropActiveShape newModel
      Nothing ->
        (newModel, cmd)


type alias XPosition =
  { x_position : Int
  }


generateNewPieceCmd : Cmd Msg
generateNewPieceCmd =
  Random.generate (\a -> RandomShapeType (Shape.decodeShapeType a)) (Random.int 0 4)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartGame ->
      ({ model | state = Playing }, fetchGameId)
    FetchGameId (Ok gameId) ->
      ({ model | gameId = Just gameId }, generateNewPieceCmd)
    FetchGameId (Err _) ->
      (model, Cmd.none)
    Tick time ->
      case model.state of
        NotStarted ->
          (model, Cmd.none)
        Playing ->
          let (newModel, cmd) = dropShapesByOnePixel model in
            (newModel, cmd)
        Over ->
          finish model
    RandomShapeType shapeType ->
        (model, fetchShapeXPos model.gameId model.blockMap shapeType)
    ShapeXPos shapeType (Ok xPos) ->
      (model, Random.generate (RandomRotate xPos shapeType) (Random.int 0 3))
    ShapeXPos shapeType (Err _) ->
      (model, Cmd.none)
    RandomRotate xPos shapeType rotateAmount ->
      let newShape = generateNewPiece xPos shapeType rotateAmount model in
        if Shape.isXPosAllowed xPos newShape then
          ({ model | shapes = (newShape :: model.shapes) }, Cmd.none)
        else
          (model, generateNewPieceCmd)
    KeyUp key ->
      if key == spaceKey then
        dropActiveShape model
      else if key == getKeyCode Left then
        (movePiece model DirectionLeft model.blockMap, Cmd.none)
      else if key == getKeyCode Right then
        (movePiece model DirectionRight model.blockMap, Cmd.none)
      else if key == getKeyCode Up then
        (rotateActiveShape model, Cmd.none)
      else if key == getKeyCode Down then
        dropShapesByOnePixel model
      else
        (model, Cmd.none)
    EmptyMsg ->
      (model, Cmd.none)


tetris : Model -> List Form
tetris model =
  case model.state of
    NotStarted ->
      [Grid.gridLines model]
    Playing ->
      (Grid.gridLines model) :: (List.map shapeToForm (model.shapes |> List.filter (\s -> s.isActive)))
    Over ->
      [ Text.fromString "GAME OVER" |> Collage.text |> move(Grid.toFloatPos (board.width//2, -board.height//2)) ]


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


startButton : Model -> Html Msg
startButton model =
  case model.state of
    NotStarted ->
      button [ onClick StartGame ] [ text "Start" ]
    _ ->
      div [] []

view model =
  div []
    [ tetrisHtml model
    , startButton model
    ]
