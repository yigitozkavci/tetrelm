import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Element exposing (toHtml)
import Collage exposing (..)
import Text
import Color exposing(red, blue)


main =
  Html.beginnerProgram { model = board, view = view, update = update }


type alias Board =
  { width : Float
  , height : Float
  , tileSize : Float
  }


board : Board
board =
  { width = 300
  , height = 600
  , tileSize = 30
  }


type Msg = Increment


update msg model =
  model


rectStyle : Int -> String
rectStyle num =
  "fill: rgb(255, 0, 0); width: 100%; height: " ++ toString num ++ ";"


type alias Block = Form


generateBlock : Board -> Float -> Float -> Block
generateBlock board x y =
  filled red (square 40) |> move (20 + x, -20 - y)


type alias Shape =
  { x : Int
  , y : Int
  , tiles : List Block
  }


tetris : Board -> List Form
tetris board =
  [ generateBlock board 10 10
  , gridLines board
  ]


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


tetrisHtml : Board -> Html Msg
tetrisHtml board =
  collage 301 601
    [ tetris board |> group |> move (-150, 300)
    ] |> toHtml


view board =
  div []
    [ tetrisHtml board
    ]
