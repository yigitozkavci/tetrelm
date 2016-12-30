module Board exposing (..)
import Element exposing (..)
import Collage exposing (Form)

type alias Board =
  { width : Float
  , height : Float
  , tileSize : Float
  }
