module Shape exposing (..)
import Location exposing (Location)
import Collage exposing (Form, filled, square, move, group, collage)
import Color exposing (red, blue)
import Board exposing (Board, board)
import Grid
import Block exposing (Block)

type Shape = L | RL | I | S


blockShapeLocations : Location -> Shape -> List Location
blockShapeLocations startingLocation shape =
  case shape of
    L ->
      List.map (Location.shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (2, 0)
        , (0, 1)
        ]
    RL ->
      List.map (Location.shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (2, 0)
        , (2, 1)
        ]
    I ->
      List.map (Location.shiftLocationBy startingLocation)
        [ (0, 0)
        , (0, 1)
        , (0, 2)
        , (0, 3)
        ]
    S ->
      List.map (Location.shiftLocationBy startingLocation)
        [ (0, 0)
        , (1, 0)
        , (0, 1)
        , (1, 1)
        ]


generateBlock : Location -> Block
generateBlock location =
  filled red (square (toFloat board.tileSize))
    |> move (Grid.toFloatPos (Location.scaleLocation board.tileSize location))


generateShape : Location -> Shape -> Form
generateShape location shape =
  List.map (\location -> generateBlock location)
    (blockShapeLocations location shape) |> group

