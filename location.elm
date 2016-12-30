module Location exposing (..)

type alias Location =
  (Int, Int)


shiftLocationBy : Location -> Location -> Location
shiftLocationBy startingLocation location =
  let (startX, startY) = startingLocation in
  let (x, y) = location in
  (startX + x, startY + y)


scaleLocation : Int -> Location -> Location
scaleLocation scale location =
  let (x, y) = location in
    ( scale//2 + x * scale
    , -scale//2 - y * scale
    )


