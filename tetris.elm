import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Element exposing (toHtml)
import Collage exposing (..)
import Text
import Color exposing(red, blue)
import Grid
import Board exposing (Board)


main =
  Html.beginnerProgram { model = board, view = view, update = update }


board : Board
board =
  { width = 300
  , height = 600
  , tileSize = 30
  }


type Msg = Increment


update msg model =
  model


type alias Block = Form


generateBlock : Board -> Float -> Float -> Block
generateBlock board x y =
  filled red (square board.tileSize) |> move (board.tileSize/2 + x * board.tileSize, -board.tileSize/2 - y * board.tileSize)


type alias Shape =
  { x : Int
  , y : Int
  , tiles : List Block
  }


tetris : Board -> List Form
tetris board =
  [ generateBlock board 2 1
  , Grid.gridLines board
  ]


tetrisHtml : Board -> Html Msg
tetrisHtml board =
  collage 301 601
    [ tetris board |> group |> move (-150, 300)
    ] |> toHtml


view board =
  div []
    [ tetrisHtml board
    ]
