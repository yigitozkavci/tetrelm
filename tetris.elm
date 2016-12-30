import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Element exposing (toHtml)
import Text
import Color exposing(red, blue)
import Grid
import Board exposing (Board, board)
import Location exposing (Location)
import Shape exposing (Shape(..))
import Collage exposing
  ( Form
  , collage
  , group
  , move
  )

main =
  Html.beginnerProgram { model = board, view = view, update = update }


type Msg = Increment


update msg model =
  model


tetris : List Form
tetris =
  [ Shape.generateShape (2, 4) L
  , Shape.generateShape (1, 7) S
  , Grid.gridLines
  ]


tetrisHtml : Html Msg
tetrisHtml =
  collage 301 601
    [ tetris |> group |> move (-150, 300)
    ] |> toHtml


view model =
  div []
    [ tetrisHtml
    ]
