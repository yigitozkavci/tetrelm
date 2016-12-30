module Grid exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Board exposing (..)


toGridLineX : Board -> Float -> Form
toGridLineX board x =
  traced { defaultLine | color = blue } (segment (board.tileSize * x, 0) (board.tileSize * x, -board.height))


toGridLineY : Board -> Float -> Form
toGridLineY board y =
  traced { defaultLine | color = blue } (segment (0, -y * board.tileSize) (board.width, -y * board.tileSize))


generateGridLines : Board -> List Form
generateGridLines board =
  List.append (generateGridLineX board [] 1) (generateGridLineY board [] 1)


generateGridLineX : Board -> List Form -> Float -> List Form
generateGridLineX board formList iterator =
  if iterator < board.width / board.tileSize + 1 then
    generateGridLineX board (toGridLineX board iterator :: formList) (iterator + 1)
  else
    formList


generateGridLineY : Board -> List Form -> Float -> List Form
generateGridLineY board formList iterator =
  if iterator < board.height / board.tileSize + 1 then
    generateGridLineY board (toGridLineY board iterator :: formList) (iterator + 1)
  else
    formList


gridLines : Board -> Form
gridLines board =
  generateGridLines board |> group

