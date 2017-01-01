module Shape exposing (..)
import Location exposing (Location)
import Collage exposing (Form, filled, square, move, group, collage)
import Color exposing (red, blue)
import Board exposing (Board, board)
import Grid
import Block exposing (Block)

type ShapeType = L | RL | I | S | T

type alias Shape =
  { x : Int
  , y : Int
  , shapeType : ShapeType
  , blockLocations : List Location
  }


initialBlockLocationsFor : ShapeType -> List Location
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
      , (1, 0)
      , (0, 0)
      , (2, 0)
      , (1, 1)
      ]


withShiftedLocations : Shape -> Shape
withShiftedLocations shape =
  { shape | blockLocations = shiftLocationsBy shape.x shape.y shape.blockLocations }


shiftLocationsBy : Int -> Int -> List Location -> List Location
shiftLocationsBy x y locations =
  List.map (\location ->
    let (locX, locY) = location in
      (x + locX, y + locY) 
  ) locations


shapeToForm : Shape -> Form
shapeToForm shape =
  List.map generateBlock (withShiftedLocations shape).blockLocations |> group


generateBlock : Location -> Block
generateBlock location =
  filled red (square (toFloat board.tileSize))
    |> move (Grid.toFloatPos (Location.scaleLocation board.tileSize location))


rotate : Shape -> Shape
rotate shape =
  shape
