module Grid exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Board exposing (..)
import Location exposing (Location)


toFloatPos : Location -> (Float, Float)
toFloatPos location =
  let (x, y) = location in
    (toFloat x, toFloat y)


intSegment : Location -> Location -> Path
intSegment beginLocation endLocation =
    segment (toFloatPos beginLocation) (toFloatPos endLocation)


toGridLineX : Int -> Form
toGridLineX x =
  traced { defaultLine | color = blue } (intSegment (board.tileSize * x, 0) (board.tileSize * x, -board.height))


toGridLineY : Int -> Form
toGridLineY y =
  traced { defaultLine | color = blue } (intSegment (0, -y * board.tileSize) (board.width, -y * board.tileSize))


generateGridLineX : List Form -> Int -> List Form
generateGridLineX formList iterator =
  if iterator < board.width // board.tileSize + 1 then
    generateGridLineX (toGridLineX iterator :: formList) (iterator + 1)
  else
    formList


generateGridLineY : List Form -> Int -> List Form
generateGridLineY formList iterator =
  if iterator < board.height // board.tileSize + 1 then
    generateGridLineY (toGridLineY iterator :: formList) (iterator + 1)
  else
    formList


generateGridLines : List Form
generateGridLines =
  List.append (generateGridLineX [] 1) (generateGridLineY [] 1)


gridLines : Form
gridLines =
  generateGridLines |> group

