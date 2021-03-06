module Shape exposing (..)
import Location
import Collage exposing (Form, filled, square, move, group, collage, outlined, defaultLine)
import Color exposing (red, blue)
import Types exposing (..)
import Grid
import Location
import List exposing (foldr)
import Matrix


dropAllowed : BlockMap -> Shape -> Bool
dropAllowed blockMap shape =
  shape.isActive &&
    (List.map (Location.dropAllowed blockMap) (withShiftedLocations shape).blockLocations
      |> foldr (&&) True)


rotateAllowed : Shape -> BlockMap -> Bool
rotateAllowed shape blockMap =
  Debug.log "Rotate allowed" (
    (withShiftedLocations (rotateBy 1 shape)).blockLocations
      |> List.all (Location.isPositionAllowed blockMap)
  )
  
positionValid : Shape -> Bool
positionValid shape =
  (List.map (Location.positionValid) (withShiftedLocations shape).blockLocations
    |> foldr (&&) True)


isXPosAllowed : Int -> Shape -> Bool
isXPosAllowed xPos shape =
  List.map Location.isXPosAllowed (withShiftedLocations shape).blockLocations
    |> foldr (&&) True


movePiece : Shape -> Direction -> BlockMap -> Shape
movePiece shape direction blockMap =
  let
    newX =
      case direction of
        DirectionLeft -> shape.x - 1
        DirectionRight -> shape.x + 1
    newShape = { shape | x = newX }
    isPositionAllowed =
      List.map (\loc -> Location.isPositionAllowed blockMap loc) (withShiftedLocations newShape).blockLocations
        |> List.foldr (&&) True
  in
    if isPositionAllowed then
      { shape | x = newX }
    else
      shape


initialBlockLocationsFor : ShapeType -> List Matrix.Location
initialBlockLocationsFor shapeType =
  case shapeType of
    L ->
      [ (0, -1)
      , (0, 0)
      , (0, 1)
      , (1, 1)
      ]
    RL ->
      [ (0, -1)
      , (0, 0)
      , (0, 1)
      , (-1, 1)
      ]
    I ->
      [ (0, -1)
      , (0, 1)
      , (0, 0)
      , (0, 2)
      ]
    S ->
      [ (1, 0)
      , (0, 1)
      , (0, 0)
      , (1, 1)
      ]
    T ->
      [ (0, 0)
      , (-1, 0)
      , (1, 0)
      , (0, -1)
      ]


withShiftedLocations : Shape -> Shape
withShiftedLocations shape =
  { shape | blockLocations = shiftLocationsBy shape.x shape.y shape.blockLocations }


shiftLocationsBy : Int -> Int -> List Matrix.Location -> List Matrix.Location
shiftLocationsBy x y locations =
  List.map (\location ->
    let (locX, locY) = location in
      (x + locX, y + locY) 
  ) locations


shapeToForm : Shape -> Form
shapeToForm shape =
  (withShiftedLocations shape).blockLocations |> List.map generateBlock |> group


generateBlock : Matrix.Location -> Block
generateBlock location =
  outlined { defaultLine | width = 3 } (square (toFloat board.tileSize))
    |> move (Grid.toFloatPos (Location.scaleLocation board.tileSize location))


rotateBy : Int -> Shape -> Shape
rotateBy rotateAmount shape =
  if rotateAmount == 0 then
    shape
  else
    rotateBy (rotateAmount - 1) { shape | blockLocations = (List.map Location.rotate shape.blockLocations) }


encodeShapeType : ShapeType -> Int
encodeShapeType shapeType =
  case shapeType of
    L ->  1
    RL -> 2
    I ->  3
    S ->  4
    T ->  5


decodeShapeType : Int -> ShapeType
decodeShapeType encodedType =
  case encodedType of
    1 -> L
    2 -> RL
    3 -> I
    4 -> S
    5 -> T
    _ -> L
