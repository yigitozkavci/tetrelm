module Types exposing (..)
import Matrix exposing (Matrix)
import Collage exposing (Form)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Http

type Msg =
  Tick Time
  | EmptyMsg
  | ShapeXPos ShapeType (Result Http.Error Int)
  | RandomRotate Int ShapeType Int
  | KeyUp KeyCode
  | RandomShapeType ShapeType


type Direction = Left | Right


type GameState = Playing | Over


type alias Model =
  { shapes : List Shape
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
  , lastDropped : Bool
  }


type ShapeType = L | RL | I | S | T

