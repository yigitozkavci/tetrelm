module Location exposing (..)
import Matrix exposing (Matrix)
import Types exposing (..)

shiftLocationBy : Matrix.Location -> Matrix.Location -> Matrix.Location
shiftLocationBy startingLocation location =
  let (startX, startY) = startingLocation in
  let (x, y) = location in
  (startX + x, startY + y)


scaleLocation : Int -> Matrix.Location -> Matrix.Location
scaleLocation scale location =
  let (x, y) = location in
    ( scale//2 + x * scale
    , -scale//2 - y * scale
    )

dropAllowed : BlockMap -> Matrix.Location -> Bool
dropAllowed blockMap location =
  let
    (x, y) = location
    result = Matrix.get (x, y + 1) blockMap
  in
    case result of
      Just value ->
        value == 0
      Nothing ->
        False


isXPosAllowed : Matrix.Location -> Bool
isXPosAllowed location =
   let (x, _) = location in
     x >= 0 && x < 10 


rotate : Matrix.Location -> Matrix.Location
rotate location =
  let (x, y) = location in
      (-y, x)


