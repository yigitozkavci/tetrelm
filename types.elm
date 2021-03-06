module Types exposing (..)
import Matrix exposing (Matrix)
import Collage exposing (Form)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Http exposing (..)

type Msg =
  Tick Time
  | EmptyMsg
  | ShapeXPos ShapeType (Result Error XPosAndRotateAmount)
  | RandomRotate Int ShapeType Int
  | KeyUp KeyCode
  | RandomShapeType ShapeType
  | StartGame
  | FetchGameId (Result Error Int)
  | FinishGame (Result Error ())


type alias XPosAndRotateAmount =
  { xPosition : Int
  , rotateAmount : Int
  }


type Direction = DirectionLeft | DirectionRight


type KeyType =
  SendPieceKey
  | RestartKey
  | Left
  | Up
  | Right
  | Down
  | Space
  | Enter


type alias GameId = Int


type alias GameTime = Int


type GameState = NotStarted | Playing | Over


type alias Model =
  { shapes : List Shape
  , blockMap : BlockMap
  , state : GameState
  , gameId : GameId
  , gameTime : GameTime
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

