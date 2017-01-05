module Types exposing (..)
import Matrix exposing (Matrix)
import Collage exposing (Form)
import Time exposing (Time)
import Keyboard exposing (KeyCode)

type Msg =
  Tick Time
  | EmptyMsg
  | RandomXPos Int
  | RandomType Int Int
  | RandomRotate Int ShapeType Int
  | KeyUp KeyCode


type Direction = Left | Right


type GameState = Playing | Over


type alias Model =
  { shapes : List Shape
  , pieceInterval : Int
  , blockMap : BlockMap
  , state : GameState
  }


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


type alias Block = Form


type alias BlockMap = Matrix Int


type alias Shape =
  { x : Int
  , y : Int
  , shapeType : ShapeType
  , blockLocations : List Matrix.Location
  , isActive : Bool
  }


type ShapeType = L | RL | I | S | T

