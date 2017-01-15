module Rest exposing (..)
import Types exposing (..)
import Http
import Json.Decode exposing(..)

type UrlType =
  CreateNewGameUrl
  | CreateMoveUrl (Maybe Int)

getUrl : UrlType -> String
getUrl urlType =
  case urlType of
    CreateNewGameUrl ->
      "http://localhost:3000/tetrelm/games/create"
    CreateMoveUrl Nothing ->
      ""
    CreateMoveUrl (Just gameId) ->
      "http://localhost:3000/tetrelm/games/" ++ toString gameId ++ "/moves/create"

createMoveJsonBody : ShapeType -> BlockMap -> Http.Body
createMoveJsonBody shapeType blockMap =
  Http.stringBody "application/json" " { \"selected_piece\": 3, \"board\": [ [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ] ] } "


xPositionDecoder : Decoder Int
xPositionDecoder =
  Json.Decode.field "x_position" Json.Decode.int


fetchGameId : Cmd Msg
fetchGameId =
  let
    request =
      Http.post (getUrl CreateNewGameUrl) Http.emptyBody (Json.Decode.field "game_id" Json.Decode.int)
  in
    Http.send FetchGameId request


fetchShapeXPos : GameId -> BlockMap -> ShapeType -> Cmd Msg
fetchShapeXPos gameId blockMap shapeType =
  let
    request =
      Http.post (getUrl <| CreateMoveUrl gameId) (createMoveJsonBody shapeType blockMap) (xPositionDecoder)
  in
    Http.send (ShapeXPos shapeType) request


