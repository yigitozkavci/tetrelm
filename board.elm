module Board exposing (..)
import Element exposing (..)
import Collage exposing (Form)

type alias Board =
  { width : Int
  , height : Int
  , tileSize : Int
  }


board : Board
board =
  { width = 300
  , height = 600
  , tileSize = 30
  }
