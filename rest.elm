module Rest exposing (..)
import Types exposing (..)
import Http
import Json.Decode exposing(..)
import Json.Encode exposing(..)
import Matrix
import Shape


type UrlType =
  CreateNewGameUrl
  | CreateMoveUrl Int
  | UpdateGameTimeUrl Int


getUrl : UrlType -> String
getUrl urlType =
  case urlType of
    CreateNewGameUrl ->
      "http://localhost:3000/tetrelm/games/create"
    CreateMoveUrl gameId ->
      "http://localhost:3000/tetrelm/games/" ++ toString gameId ++ "/moves/create"
    UpdateGameTimeUrl gameId ->
      "http://localhost:3000/tetrelm/games/" ++ toString gameId ++ "/update"


encodeSelectedPiece : ShapeType -> Json.Encode.Value
encodeSelectedPiece shapeType =
  Shape.encodeShapeType shapeType 
    |> Json.Encode.int


encodeBoard : BlockMap -> Json.Encode.Value
encodeBoard blockMap =
  blockMap
    |> Matrix.map Json.Encode.int
    |> Matrix.toList
    |> List.map Json.Encode.list
    |> Json.Encode.list


createMoveJsonBody : ShapeType -> BlockMap -> Http.Body
createMoveJsonBody shapeType blockMap =
  Json.Encode.object [("selected_piece", encodeSelectedPiece shapeType), ("board", encodeBoard blockMap)]
    |> Http.jsonBody


put : String -> Http.Body -> Http.Request ()
put url body =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }


updateGameTime : GameTime -> GameId -> Cmd Msg
updateGameTime gameTime gameId =
  let
    request =
      put (getUrl <| UpdateGameTimeUrl gameId) (Http.jsonBody (Json.Encode.object [("time", Json.Encode.int gameTime)]))
  in
    Http.send FinishGame request


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


